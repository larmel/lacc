#include <stdio.h>
#include <stdlib.h>
#include "lcc.h"


int main(int argc, char* argv[])
{
	FILE *input = stdin;
	struct token t;

	if (argc == 2) {
		FILE *f;
		if (f = fopen(argv[1], "r")) {
			input = f;
		}
	}

	while (get_token(input, &t)) {
		printf("token( %s, %d )\n", (char*) t.value, t.type);
	}

	return EXIT_SUCCESS;
}
