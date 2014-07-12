#include <stdio.h>

#ifdef AUX
#include <pthread.h>
#include <unistd.h>
#endif

#include "altR.h"


void *runR(void *ignored)
{
	puts("-----> C: Starting R...");
	altR_initR();
	while (altR_do1LineR()) {
		;
	}
	altR_endR();
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
