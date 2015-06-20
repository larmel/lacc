#include "error.h"
#include "ir.h"
#include "string.h"
#include "symbol.h"
#include "input.h"
#include "preprocess.h"

#include <stdio.h>
#include <unistd.h>

int VERBOSE = 0;

void help()
{
    fprintf(stderr, "Usage: lcc [-S] [-v] [-I <path>] [-o <file>] [file]\n");
}

extern struct decl *parse();
extern void fdotgen(FILE *, const struct decl *);
extern void fassemble(FILE *, const struct decl *);

int main(int argc, char* argv[])
{
    char *input = NULL;
    int c, assembly = 0;
    FILE *output = stdout;

    while ((c = getopt(argc, argv, "So:vI:")) != -1) {
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
            case 'I':
                add_include_search_path(optarg);
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

    push_scope(&ns_ident);
    push_scope(&ns_tag);

    register_builtin_definitions();
    register_builtin_types(&ns_ident);

    while (1) {
        struct decl *fun = parse();
        if (errors || !fun) {
            if (errors) {
                error("Aborting because of previous %s.",
                    (errors > 1) ? "errors" : "error");
            }
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

    pop_scope(&ns_tag);
    pop_scope(&ns_ident);

    if (assembly) {
        output_definitions(output);
        output_strings(output);
    }

    if (VERBOSE) {
        dump_symtab(&ns_ident);
        dump_symtab(&ns_tag);
    }

    return errors;
}
