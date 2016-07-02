#include "backend/compile.h"
#include "parser/parse.h"
#include "parser/symtab.h"
#include "preprocessor/preprocess.h"
#include "preprocessor/input.h"
#include "preprocessor/macro.h"
#include "util/argparse.h"
#include <lacc/cli.h>
#include <lacc/ir.h>

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

static enum compile_target target;
static char *program;
static char *input;
static FILE *output;
static enum c_standard {
    STD_C89
} standard;

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
        target = TARGET_x86_64_ELF;
        break;
    case 'S':
        target = TARGET_x86_64_ASM;
        break;
    case 'E':
        target = TARGET_NONE;
        break;
    case 'v':
        verbose_level += 1;
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
        standard = STD_C89;
    } else {
        fprintf(stderr, "Unrecognized option %s.\n", std);
        exit(1);
    }
}

static void parse_program_arguments(int argc, char *argv[])
{
    int c;
    struct option optv[] = {
        {"-S", &flag},
        {"-E", &flag},
        {"-c", &flag},
        {"-v", &flag},
        {"--help", &help},
        {"-o:", &open_output_handle},
        {"-I:", &add_include_search_path},
        {"-std=", &set_c_std}
    };

    program = argv[0];
    standard = STD_C89;
    target = TARGET_IR_DOT;
    output = stdout;
    c = parse_args(sizeof(optv)/sizeof(optv[0]), optv, argc, argv);
    if (c == argc - 1) {
        input = argv[c];
    } else if (c < argc - 1) {
        help(argv[0]);
        exit(1);
    }
}

int main(int argc, char *argv[])
{
    struct definition *def;
    const struct symbol *sym;

    parse_program_arguments(argc, argv);

    /* Add default search paths last, with lowest priority. These are
     * searched after anything specified with -I, and in the order
     * listed here. */
    add_include_search_path("/usr/local/include");
    add_include_search_path("/usr/lib/lacc/include");
    add_include_search_path("/usr/include/x86_64-linux-gnu");
    add_include_search_path("/usr/include");

    init(input);
    register_builtin_definitions();
    set_compile_target(output, target);

    if (target == TARGET_NONE) {
        preprocess(output);
    } else {
        push_scope(&ns_ident);
        push_scope(&ns_tag);
        register_builtin_types(&ns_ident);

        while ((def = parse()) != NULL) {
            if (errors) {
                error("Aborting because of previous %s.",
                    (errors > 1) ? "errors" : "error");
                break;
            }
            compile(def);
        }

        while ((sym = yield_declaration(&ns_ident)) != NULL) {
            declare(sym);
        }

        if (verbose_level) {
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

    return errors;
}
