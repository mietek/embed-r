#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>


pthread_t altR_main_thread = 0;


void handle_main_thread_signal(int sig)
{
	if (pthread_self() != altR_main_thread) {
		puts("handle_main_thread_signal called in non-main thread");
		return;
	}

	R_ProcessEvents();
}


void *trigger_main_thread_signal(void *ignored)
{
	for (;;) {
		pthread_kill(altR_main_thread, SIGUSR1);
		usleep(100000);
	}
}


void altR_init()
{
	struct sigaction action_info;
	pthread_t trigger_thread;

	altR_main_thread = pthread_self();

	action_info.sa_handler = handle_main_thread_signal;
	action_info.sa_flags = SA_RESTART;
	sigemptyset(&action_info.sa_mask);
	sigaction(SIGUSR1, &action_info, NULL);

	pthread_create(&trigger_thread, NULL, trigger_main_thread_signal, NULL);
}


int altR_do1Line()
{
	static char input[256];
	SEXP inputexpr, parsedexpr;
	ParseStatus status;

	if (!fgets(input, sizeof(input), stdin)) {
		return 0;
	}
	input[strlen(input) - 1] = 0;
	fprintf(stderr, "-----> C: Parsing '%s'...\n", input);

	inputexpr = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(inputexpr, 0, mkChar(input));

	parsedexpr = PROTECT(R_ParseVector(inputexpr, -1, &status, R_NilValue));
	if (status != PARSE_OK) {
		fprintf(stderr, "-----> C: Failed to parse '%s'\n", input);
		UNPROTECT(2);
		return 0;
	}

	for (int i = 0; i < length(parsedexpr); i++) {
		eval(VECTOR_ELT(parsedexpr, i), R_GlobalEnv);
	}

	UNPROTECT(2);
	return 1;
}
