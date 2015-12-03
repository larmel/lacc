#ifndef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* getopt */
#endif
#include "backend/compile.h"
#include "parser/cfg.h"
#include "parser/parse.h"
#include "parser/symtab.h"
#include "preprocessor/preprocess.h"
#include "preprocessor/input.h"
#include "preprocessor/macro.h"
#include <lacc/cli.h>
#include <lacc/string.h>

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

static char *input;
static FILE *output;

static void help(const char *prog)
{
    fprintf(stderr, "Usage: %s [-S] [-E] [-v] [-I <path>] [-o <file>] <file>\n",
        prog);
}

static enum compile_target parse_args(int argc, char *argv[])
{
    enum compile_target target;
    int c;

    target = TARGET_IR_DOT;
    output = stdout;

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
            exit(1);
        }
    }

    if (optind == argc - 1)
        input = argv[optind];
    else if (optind < argc - 1) {
        help(argv[0]);
        exit(1);
    }

    return target;
}

int main(int argc, char *argv[])
{
    enum compile_target target = parse_args(argc, argv);

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
