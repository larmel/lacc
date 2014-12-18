#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    const char *filename;

    if (argc == 2) {
        filename = argv[1];
    } else {
        fprintf(stderr, "fatal error: missing input file");
        return EXIT_FAILURE;
    }

    /* preprocessor gets file reference */
    preprocess(filename);

    /* first pass, generate cfg */
    compile();

    dump_symtab();

    return EXIT_SUCCESS;
}
