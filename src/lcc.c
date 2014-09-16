#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern size_t line_number;

static const char * describe(enum token_type);

int main(int argc, char* argv[])
{
    char *line;
    const char *filename;

    if (argc == 2) {
        filename = argv[1];
    } else {
        fprintf(stderr, "fatal error: missing input file");
        exit(0);
    }

    /* stitch together parsing and tokenization */
    char *filebuf;
    size_t filesize;
    FILE *p = open_memstream(&filebuf, &filesize);

    init_preprocessing(filename);

    /* preprocess to temp file buffer */
    while (getprepline(&line) != -1) {
        fputs(line, p);
        printf("%03d  %s", (int)line_number, line);
    }

    /* parse */
    init_parsing(p);
    codegen();
    //node_t *tree = parse(p);
    fclose(p);

    puts("");
    dump_symtab();
    puts("");

    printir(stdout);

    return EXIT_SUCCESS;
}

static const char *
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
    return "";
}
