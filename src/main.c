#ifndef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* getopt */
#endif
#include "backend/compile.h"
#include "core/cli.h"
#include "core/parse.h"
#include "core/string.h"
#include "core/symbol.h"
#include "frontend/input.h"
#include "frontend/preprocess.h"

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

static void help(const char *prog)
{
    fprintf(stderr, "Usage: %s [-S] [-E] [-v] [-I <path>] [-o <file>] <file>\n",
        prog);
}

int main(int argc, char* argv[])
{
    enum compile_target target = TARGET_IR_DOT;
    char *input = NULL;
    FILE *output = stdout;
    int c;

    /* Handle command line parameters. */
    while ((c = getopt(argc, argv, "SEo:vI:")) != -1) {
        switch (c) {
        case 'S':
            target = TARGET_x86_64_ASM;
            break;
        case 'E':
            target = TARGET_NONE;
            break;
        case 'o':
            output = fopen(optarg, "w");
            break;
        case 'v':
            verbose_level += 1;
            break;
        case 'I':
            add_include_search_path(optarg);
            break;
        default:
            help(argv[0]);
            return 1;
        }
    }

    if (optind == argc - 1) {
        input = argv[optind];
    } else if (optind < argc - 1) {
        help(argv[0]);
        return 1;
    }

    /* Add default search paths last, with lowest priority. These are searched
     * after anything specified with -I. */
    add_include_search_path("/usr/include");
    add_include_search_path("/usr/local/include");

    init(input);
    register_builtin_definitions();
    set_compile_target(output, target);

    if (target == TARGET_NONE) {
        preprocess(output);
    } else {
        push_scope(&ns_ident);
        push_scope(&ns_tag);
        register_builtin_types(&ns_ident);

        while (parse() && !errors)
            compile_cfg(&current_cfg);

        if (errors)
            error("Aborting because of previous %s.",
                (errors > 1) ? "errors" : "error");

        compile_symbols(
            get_tentative_definitions(&ns_ident));

        if (verbose_level) {
            output_symbols(stdout, &ns_ident);
            output_symbols(stdout, &ns_tag);
        }

        pop_scope(&ns_tag);
        pop_scope(&ns_ident);
    }

    if (output != stdout)
        fclose(output);

    return errors;
}
