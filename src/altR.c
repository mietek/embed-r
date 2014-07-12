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

#include <R.h>
#include <Rembedded.h>
#include <Rinterface.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef ALTR_POSIX
#include <setjmp.h>
#include <signal.h>
#define JMP_BUF      sigjmp_buf
#define SET_JMP(buf) sigsetjmp(buf, 0)
#endif

#ifdef ALTR_CYGWIN
#include <psignal.h>
#include <windows.h>
#define JMP_BUF      jmp_buf
#define SET_JMP(buf) setjmp(buf)
#endif


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
 * appropriate value.  Unfortunately, R_Toplevel is not a visible symbol in
 * the R dynamic library on Linux and Cygwin, only on Darwin.
 * To work around this, after partially initialising R, we search the memory
 * around R_GlobalContext for the distinctive pattern of a freshly
 * initialised R_Toplevel.
 * The following definition is copied from the R private header Defn.h.
 */

typedef struct RCNTXT {
    void    *nextcontext;
    int     callflag;
    JMP_BUF cjmpbuf;
    int     cstacktop;
    int     evaldepth;
    SEXP    promargs;
    SEXP    callfun;
    SEXP    sysparent;
    SEXP    call;
    SEXP    cloenv;
    SEXP    conexit;
    void    (*cend)(void *);
    void    *cenddata;
    void    *vmax;
    int     intsusp;
} RCNTXT;

static RCNTXT *_altR_Toplevel;

static void _altR_findRToplevel()
{
	RCNTXT *ctxt;
	void   *ptr    = R_GlobalContext - 1024;
	void   *ptrEnd = R_GlobalContext + 1024;

#ifdef ALTR_DARWIN
	ctxt = dlsym(RTLD_SELF, "R_Toplevel");
	if (ctxt) {
		fprintf(stderr, "-----> C: Found exported R toplevel context\n");
		_altR_Toplevel = ctxt;
		return;
	}
#endif

	while (ptr < ptrEnd) {
		ctxt = ptr;
	        if (	   ctxt->nextcontext  == NULL
			&& ctxt->callflag     == 0
			&& ctxt->cstacktop    == 0
			&& ctxt->promargs     == R_NilValue
			&& ctxt->callfun      == R_NilValue
			&& ctxt->sysparent    == R_BaseEnv
			&& ctxt->call         == R_NilValue
			&& ctxt->cloenv       == R_BaseEnv
			&& ctxt->conexit      == R_NilValue
			&& ctxt->cend         == NULL
			&& ctxt->vmax         == NULL
			&& ctxt->intsusp      == FALSE
		) {
			_altR_Toplevel = ctxt;
			fprintf(stderr, "-----> C: Found hidden R toplevel context\n");
			return;
		}
		ptr++;
	}

	fprintf(stderr, "-----> C: Failed to find R toplevel context\n");
	abort();
}


/* R is designed to be run in the main thread.  Otherwise, issues arise with
 * the stack-checking mechanism and with GUI functionality.
 * In order to ensure R can handle GUI events even when the main thread is
 * controlled by a third-party, we want to interrupt the main thread every
 * once in a while and call R_ProcessEvents from its thread context.
 * To this end, we overwrite the R SIGUSR1 signal handler, and we spawn a new
 * thread, which triggers SIGUSR1 on the main thread every 100 ms.
 */

static void _altR_handlePokeSignal(int signal)
{
	R_ProcessEvents();
}

static void *_altR_triggerPokeSignal(void *mainThreadPtr)
{
	struct timespec sleep100ms = { 0, 100000000 };

	for (;;) {
		pthread_kill(*(pthread_t *)mainThreadPtr, SIGUSR1);
		nanosleep(&sleep100ms, NULL);
	}

	return NULL;
}


/* Initialise the embedded R instance.
 */
void altR_initR()
{
	static const char *args[] = { "altR", "--silent", "--vanilla" };
	struct sigaction  signalAction;
	static pthread_t  mainThread;
	pthread_t         pokeThread;

	Rf_initEmbeddedR(sizeof(args) / sizeof(args[0]), (char **)args);

	_altR_findRToplevel();

	signalAction.sa_handler = _altR_handlePokeSignal;
	signalAction.sa_flags = SA_RESTART;
	sigemptyset(&signalAction.sa_mask);
	sigaction(SIGUSR1, &signalAction, NULL);

	mainThread = pthread_self();
	pthread_create(&pokeThread, NULL, _altR_triggerPokeSignal, &mainThread);
}


/* End the embedded R instance.
 */
void altR_endR()
{
	Rf_endEmbeddedR(0);
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

	--jmpReturn;
	SET_JMP(_altR_Toplevel->cjmpbuf);
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
	fprintf(stderr, "-----> C: Parsing '%s'...\n", input);

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
