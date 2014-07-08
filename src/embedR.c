#include <R.h>
#include <Rembedded.h>
#include <stdio.h>

#ifdef AUX
#include <pthread.h>
#include <unistd.h>
#endif

#include "altR.h"


void *runR(void *ignored)
{
	char *args[] = { "embedR", "--interactive", "--silent", "--vanilla" };
	puts("-----> C: Starting R...");
	Rf_initEmbeddedR(sizeof(args) / sizeof(args[0]), args);
	while (altR_do1Line()) {
		;
	}
	Rf_endEmbeddedR(0);
	puts("-----> C: Exiting R...");
	return NULL;
}


int main()
{
#ifdef AUX
	pthread_t tid;
	pthread_create(&tid, NULL, runR, NULL);
	usleep(1000000);
#else
	runR(NULL);
#endif
	return 0;
}
