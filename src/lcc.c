#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern size_t line_number;

int main(int argc, char* argv[])
{
	char *line;
	FILE *input = stdin;
	const char *filename;
	char *directory = ".";
	struct token t;

	if (argc == 2) {
		filename = argv[1];
		FILE *f;
		char *lastsep = strrchr(filename, '/');
		if (lastsep != NULL) {
			directory = calloc(lastsep - filename + 1, sizeof(char));
			strncpy(directory, filename, lastsep - filename);
		}
		if (f = fopen(filename, "r")) {
			input = f;
		}
	}
	printf("filename: %s\n", filename);
	printf("directory: %s\n", directory);

	init_preprocessing(input, directory);

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
