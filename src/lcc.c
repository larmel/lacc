#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern size_t line_number;

int main(int argc, char* argv[])
{
    char *line;
    const char *filename;

    char *filebuf;
    size_t filesize;
    FILE *p;

    if (argc == 2) {
        filename = argv[1];
    } else {
        fprintf(stderr, "fatal error: missing input file");
        exit(0);
    }

    /* stitch together parsing and tokenization */
    p = open_memstream(&filebuf, &filesize);

    init_preprocessing(filename);

    /* preprocess to temp file buffer */
    while (getprepline(&line) != -1) {
        fputs(line, p);
        printf("%03d  %s", (int)line_number, line);
    }

    /* parse */
    init_parsing(p);
    codegen();
    fclose(p);

    puts("");
    dump_symtab();
    puts("");

    printir(stdout);

    return EXIT_SUCCESS;
}
