#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    const char *filename;
    FILE *cfg, *code;

    if (argc == 2) {
        filename = argv[1];
    } else {
        fprintf(stderr, "fatal error: missing input file");
        return EXIT_FAILURE;
    }

    cfg  = fopen("cfg.dot", "w");
    code = fopen("a.s", "w");

    /* preprocessor gets file reference */
    preprocess(filename);

    /* generate cfg and assemble */
    compile(cfg, code);

    dump_symtab();

    return EXIT_SUCCESS;
}
