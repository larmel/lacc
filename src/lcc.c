#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern size_t line_number;

const char * describe(enum token_type);

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

	char *filebuf;
	size_t filesize;
	FILE *p = open_memstream(&filebuf, &filesize);

	init_preprocessing(input, directory);

	/* preprocess to temp file buffer */
	while (getprepline(&line) != -1) {
		fputs(line, p);
    	printf("%d\t%s", (int)line_number, line);
	}

	/* tokenize */
	while (get_token(p, &t)) {
		printf("token( %s, %s )\n", (char*) t.value, describe(t.type));
	}

	fclose(p);

	return EXIT_SUCCESS;
}

const char *
describe(enum token_type t)
{
	if (   t == AUTO || t == BREAK || t == CASE || t == CHAR 
		|| t == CONST || t == CONTINUE || t == DEFAULT || t == DO
		|| t == DOUBLE || t == ELSE || t == ENUM || t == EXTERN
		|| t == FLOAT || t == FOR || t == GOTO || t == IF
		|| t == INT || t == LONG || t == REGISTER || t == RETURN
		|| t == SHORT || t == SIGNED || t == SIZEOF || t == STATIC
		|| t == STRUCT || t == SWITCH || t == TYPEDEF || t == UNION
		|| t == UNSIGNED || t == VOID || t == VOLATILE || t == WHILE
	   )
		return "keyword";
	if (t == INTEGER)
		return "number";
	if (t == IDENTIFIER)
		return "identifier";
	if (t == STRING)
		return "string";
	return "misc";
}
