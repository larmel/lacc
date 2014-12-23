#include "lcc.h"

#include <stdio.h>
#include <unistd.h>

void help()
{
    fprintf(stderr, "Usage: lcc [-S] [-o <file>] [file]\n");
}

int main(int argc, char* argv[])
{
    const char *input = NULL;
    int c, assembly = 0;
    FILE *output = stdout;

    while ((c = getopt(argc, argv, "So:")) != -1) {
        switch (c) {
            case 'S':
                assembly = 1;
                break;
            case 'o':
                output = fopen(optarg, "w");
                break;
            default:
                help();
                return 1;
        }
    }

    if (optind == argc - 1) {
        input = argv[optind];
    } else if (optind < argc - 1) {
        help();
        return 1;
    }

    init(input);

    push_scope();
    while (1) {
        struct function *fun = parse();
        if (!fun) break;
        if (assembly)
            fassemble(output, fun);
        else
            fdotgen(output, fun);
        break;
    }
    pop_scope();

    dump_symtab();

    return EXIT_SUCCESS;
}
