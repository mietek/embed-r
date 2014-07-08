#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <stdio.h>
#include <string.h>


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
