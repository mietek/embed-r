#include <R.h>
#include <Rembedded.h>
#include <stdio.h>

#ifdef AUX
#include <pthread.h>
#include <unistd.h>
#endif


void *runR(void *ignored)
{
	char *args[] = { "cembedR", "--interactive", "--silent", "--vanilla" };
	Rf_initEmbeddedR(sizeof(args) / sizeof(args[0]), args);
	R_ReplDLLinit();
	while (R_ReplDLLdo1() > 0) {
		;
	}
	Rf_endEmbeddedR(0);
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
