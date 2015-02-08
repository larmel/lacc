#include "error.h"
#include "ir.h"
#include "symbol.h"

#include <stdio.h>
#include <unistd.h>

int VERBOSE = 0;

void help()
{
    fprintf(stderr, "Usage: lcc [-S] [-v] [-o <file>] [file]\n");
}

extern void init(const char *);
extern function_t *parse();
extern void fdotgen(FILE *, const function_t *);
extern void fassemble(FILE *, const function_t *);

int main(int argc, char* argv[])
{
    const char *input = NULL;
    int c, assembly = 0;
    FILE *output = stdout;

    while ((c = getopt(argc, argv, "So:v")) != -1) {
        switch (c) {
            case 'S':
                assembly = 1;
                break;
            case 'o':
                output = fopen(optarg, "w");
                break;
            case 'v':
                VERBOSE = 1;
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
        function_t *fun = parse();
        if (errors || !fun) {
            break;
        }
        if (fun) {
            if (assembly) {
                fassemble(output, fun);
            } else {
                fdotgen(output, fun);
            }
            cfg_finalize(fun);
        }
    }
    pop_scope();

    if (VERBOSE) {
        dump_symtab();
    }

    return errors;
}
