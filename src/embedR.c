#include <stdio.h>

#include "altR.h"


void runR()
{
	puts("-----> C: Starting R");
	altR_initR();
	while (altR_do1LineR()) {
		;
	}
	altR_endR();
	puts("-----> C: Exiting R");
}


int main()
{
	runR();
	return 0;
}
