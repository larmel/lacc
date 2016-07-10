#include "backend/compile.h"
#include "parser/parse.h"
#include "parser/symtab.h"
#include "preprocessor/preprocess.h"
#include "preprocessor/input.h"
#include "preprocessor/macro.h"
#include "util/argparse.h"
#include <lacc/context.h>
#include <lacc/ir.h>

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

const char *program;
static FILE *output;

static void help(const char *arg)
{
    fprintf(
        stderr,
        "Usage: %s [-(S|E|c)] [-v] [-I <path>] [-o <file>] <file>\n",
        program);
    exit(1);
}

static void flag(const char *arg)
{
    switch (*arg) {
    case 'c':
        context.target = TARGET_x86_64_ELF;
        break;
    case 'S':
        context.target = TARGET_x86_64_ASM;
        break;
    case 'E':
        context.target = TARGET_NONE;
        break;
    case 'v':
        context.verbose += 1;
        break;
    case 'w':
        context.suppress_warning = 1;
        break;
    default:
        assert(0);
        break;
    }
}

static void open_output_handle(const char *file)
{
    output = fopen(file, "w");
}

static void set_c_std(const char *std)
{
    if (!strcmp("c89", std)) {
        context.standard = STD_C89;
    } else if (!strcmp("c99", std)) {
        context.standard = STD_C99;
    } else {
        fprintf(stderr, "Unrecognized option %s.\n", std);
        exit(1);
    }
}

static char *parse_program_arguments(int argc, char *argv[])
{
    int c;
    char *input;
    struct option optv[] = {
        {"-S", &flag},
        {"-E", &flag},
        {"-c", &flag},
        {"-v", &flag},
        {"-w", &flag},
        {"--help", &help},
        {"-o:", &open_output_handle},
        {"-I:", &add_include_search_path},
        {"-std=", &set_c_std}
    };

    program = argv[0];
    output = stdout;
    context.standard = STD_C89;
    context.target = TARGET_IR_DOT;
    c = parse_args(sizeof(optv)/sizeof(optv[0]), optv, argc, argv);
    if (c == argc - 1) {
        input = argv[c];
    } else if (c < argc - 1) {
        help(argv[0]);
        exit(1);
    }

    return input;
}

int main(int argc, char *argv[])
{
    char *input;
    struct definition *def;
    const struct symbol *sym;

    input = parse_program_arguments(argc, argv);

    /* Add default search paths last, with lowest priority. These are
     * searched after anything specified with -I, and in the order
     * listed here. */
    add_include_search_path("/usr/local/include");
    add_include_search_path("/usr/lib/lacc/include");
    add_include_search_path("/usr/include/x86_64-linux-gnu");
    add_include_search_path("/usr/include");

    init(input);
    register_builtin_definitions();
    set_compile_target(output);

    if (context.target == TARGET_NONE) {
        preprocess(output);
    } else {
        push_scope(&ns_ident);
        push_scope(&ns_tag);
        register_builtin_types(&ns_ident);

        while ((def = parse()) != NULL) {
            if (context.errors) {
                error("Aborting because of previous %s.",
                    (context.errors > 1) ? "errors" : "error");
                break;
            }
            compile(def);
        }

        while ((sym = yield_declaration(&ns_ident)) != NULL) {
            declare(sym);
        }

        if (context.verbose) {
            output_symbols(stdout, &ns_ident);
            output_symbols(stdout, &ns_tag);
        }

        flush();
        pop_scope(&ns_tag);
        pop_scope(&ns_ident);
    }

    if (output != stdout) {
        fclose(output);
    }

    return context.errors;
}
