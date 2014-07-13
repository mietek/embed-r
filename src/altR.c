#ifdef __linux
#define ALTR_LINUX 1
#define ALTR_POSIX 1
#elif __APPLE__
#define ALTR_DARWIN 1
#define ALTR_POSIX  1
#elif defined(_WIN64) || defined(_WIN32)
#define ALTR_CYGWIN 1
#endif

#define _POSIX_C_SOURCE 200809L

#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <sys/select.h>
#include <time.h>

#ifdef ALTR_POSIX
#include <setjmp.h>
#include <signal.h>
#define JMP_BUF     sigjmp_buf
#define SETJMP(buf) sigsetjmp(buf, 0)
#endif

#ifdef ALTR_CYGWIN
#include <psignal.h>
#include <windows.h>
#define JMP_BUF     jmp_buf
#define SETJMP(buf) setjmp(buf)
#endif

#include <R.h>
#include <Rembedded.h>
#include <Rinterface.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <R_ext/eventloop.h>


/* SA_RESTART is not exported for POSIX code in some versions of glibc:
 * https://sourceware.org/bugzilla/show_bug.cgi?id=12252
 */
#ifdef ALTR_LINUX
#ifndef SA_RESTART
#define SA_RESTART 0x10000000
#endif
#endif


/* We disable POSIX mode to get RTLD_SELF, as we only use it on Darwin.
 */
#ifdef ALTR_DARWIN
#undef _POSIX_C_SOURCE
#include <dlfcn.h>
#endif


/* R uses setjmp() and longjmp() internally to recover from errors.
 * In order to properly initialise R, we must set R_Toplevel.jmp_buf to an
 * appropriate value.  If this is not done, then e.g. pressing Ctrl+C during a
 * lengthy R operation results in a crash.
 * Unfortunately, R_Toplevel is not a visible symbol in the R dynamic library
 * on Linux and Cygwin, but only on Darwin.  To work around this, after
 * partially initialising R, we follow the context chain from R_GlobalContext
 * to R_Toplevel.
 * The following definition is extracted from the R private header Defn.h.
 */

typedef struct RCNTXT {
    struct RCNTXT *nextcontext;
    int           callflag;
    JMP_BUF       cjmpbuf;
} RCNTXT;

#define CTXT_TOPLEVEL 0

static RCNTXT *_altR_toplevel;

static void _altR_findRToplevel()
{
	RCNTXT *context;

#ifdef ALTR_DARWIN
	context = dlsym(RTLD_SELF, "R_Toplevel");
	if (context) {
		fprintf(stderr, "-----> C: Found exported R toplevel context\n");
		_altR_toplevel = context;
		return;
	}
#endif

	context = R_GlobalContext;
	while (context) {
		if (context->callflag == CTXT_TOPLEVEL) {
			fprintf(stderr, "-----> C: Found hidden R toplevel context\n");
			_altR_toplevel = context;
			return;
		}
		context = context->nextcontext;
	}

	fprintf(stderr, "-----> C: Failed to find R toplevel context\n");
	abort();
}


/* R is designed to be run in the main thread.  Otherwise, issues arise with
 * the stack-checking mechanism and with GUI functionality.
 * In order to ensure R can handle GUI events even when the main thread is
 * controlled by a third-party, we want to interrupt the main thread every
 * once in a while and call R from the context of the main thread.
 * To this end, we overwrite the R SIGUSR1 signal handler and spawn a new
 * thread, which triggers SIGUSR1 on the main thread every 100 ms.
 */

static pthread_t _altR_mainThread;
static int 	 _altR_donePokingR;

static void _altR_pokeR()
{
	R_runHandlers(R_InputHandlers, R_checkActivity(0, 0));
	R_ProcessEvents();
}

static void *_altR_keepPokingR()
{
	static const struct timespec sleep100ms = { 0, 100000000 };

	while (!_altR_donePokingR) {
		pthread_kill(_altR_mainThread, SIGUSR1);
		nanosleep(&sleep100ms, NULL);
	}

	return NULL;
}


static int _altR_inited = 0;

/* Initialise the embedded R instance.
 */
void altR_initR()
{
	static const char *args[] = { "altR", "--silent", "--vanilla" };
	struct sigaction  pokeSignal;
	pthread_t         pokeThread;

	if (_altR_inited++) {
		fprintf(stderr, "-----> C: Already initialized\n");
		return;
	}
	Rf_initEmbeddedR(sizeof(args) / sizeof(args[0]), (char **)args);

	initStdinHandler();

	_altR_findRToplevel();

	_altR_mainThread  = pthread_self();
	_altR_donePokingR = 0;

	pokeSignal.sa_handler = _altR_pokeR;
	pokeSignal.sa_flags = SA_RESTART;
	sigemptyset(&pokeSignal.sa_mask);
	sigaction(SIGUSR1, &pokeSignal, NULL);

	pthread_create(&pokeThread, NULL, _altR_keepPokingR, NULL);
}


/* End the embedded R instance.
 */
void altR_endR()
{
	if (!_altR_inited) {
		fprintf(stderr, "-----> C: Not initialized yet\n");
		return;
	}

	_altR_donePokingR = 1;

	Rf_endEmbeddedR(0);
	_altR_inited = 0;
}


/* Read one line of R input from stdin, parse it, and evaluate it.
 * Returns 0 on EOF.  Otherwise, returns 1.
 */
int altR_do1LineR()
{
	static char input[256];
	SEXP        inputExpr;
	SEXP        parsedExpr;
	ParseStatus status;
	int         jmpReturn = 0;

	if (!_altR_inited) {
		fprintf(stderr, "-----> C: Not initialized yet\n");
		return 0;
	}

	--jmpReturn;
	SETJMP(_altR_toplevel->cjmpbuf);
	if (++jmpReturn) {
		fprintf(stderr, "-----> C: Interrupted.\n");
		return 1;
	}

	if (!fgets(input, sizeof(input), stdin)) {
		if (feof(stdin)) {
			return 0;
		}
		perror("fgets");
		abort();
	}
	input[strlen(input) - 1] = 0;
	fprintf(stderr, "-----> C: Parsing '%s'\n", input);

	inputExpr = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(inputExpr, 0, mkChar(input));

	parsedExpr = PROTECT(R_ParseVector(inputExpr, -1, &status, R_NilValue));
	if (status != PARSE_OK) {
		fprintf(stderr, "-----> C: Failed to parse '%s'\n", input);
		UNPROTECT(2);
		return 0;
	}

	for (int i = 0; i < length(parsedExpr); i++) {
		eval(VECTOR_ELT(parsedExpr, i), R_GlobalEnv);
	}

	UNPROTECT(2);
	return 1;
}
