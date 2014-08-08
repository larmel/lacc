#include <stdio.h>
#include <stdlib.h>
#include "lcc.h"

extern size_t line_number;

int main(int argc, char* argv[])
{
	char *line;
	FILE *input = stdin;
	struct token t;

	if (argc == 2) {
		FILE *f;
		if (f = fopen(argv[1], "r")) {
			input = f;
		}
	}

	// preprocess
	init_preprocessing(input);

	while (getprepline(&line) != -1) {
		/* ready for tokenization */
    	printf("%d\t%s", (int)line_number, line);
	}

	return EXIT_SUCCESS;

	// tokenize
	// while (get_token(input, &t)) {
	// 	printf("token( %s, %d )\n", (char*) t.value, t.type);
	// }

	// return EXIT_SUCCESS;
}
