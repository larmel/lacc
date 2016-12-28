#define _XOPEN_SOURCE 500
#include "backend/compile.h"
#include "optimizer/optimize.h"
#include "parser/parse.h"
#include "parser/symtab.h"
#include "preprocessor/preprocess.h"
#include "preprocessor/input.h"
#include "preprocessor/macro.h"
#include "util/argparse.h"
#include <lacc/context.h>
#include <lacc/ir.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <unistd.h>

/*
 * Configurable location of implementation defined standard library
 * headers. This is set in the makefile, by default pointing to files
 * from the source tree under /include/stdlib/.
 */
#ifndef LACC_STDLIB_PATH
# define LACC_STDLIB_PATH "/usr/local/lib/lacc/include"
#endif

const char *program;
static FILE *output;
static int optimization_level;

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

static void set_optimization_level(const char *level)
{
    assert(isdigit(level[2]));
    optimization_level = level[2] - '0';
}

static void define_macro(const char *arg)
{
    static char line[1024];
    char *sep;

    sep = strchr(arg, '=');
    if (sep) {
        snprintf(line, sizeof(line), "#define %s", arg);
        *(line + 8 + (sep - arg)) = ' ';
    } else {
        snprintf(line, sizeof(line), "#define %s 1", arg);
    }

    inject_line(line);
}

static char *parse_program_arguments(int argc, char *argv[])
{
    int c;
    char *input = NULL;
    struct option optv[] = {
        {"-S", &flag},
        {"-E", &flag},
        {"-c", &flag},
        {"-v", &flag},
        {"-w", &flag},
        {"--help", &help},
        {"-o:", &open_output_handle},
        {"-I:", &add_include_search_path},
        {"-O0", &set_optimization_level},
        {"-O1", &set_optimization_level},
        {"-O2", &set_optimization_level},
        {"-O3", &set_optimization_level},
        {"-std=", &set_c_std},
        {"-D:", &define_macro}
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

/*
 * Register compiler internal builtin symbols, that are assumed to
 * exists by standard library headers.
 */
static void register_builtin_declarations(void)
{
    inject_line("void *memcpy(void *dest, const void *src, unsigned long n);");
    inject_line("void __builtin_va_start(void);");
    inject_line("void __builtin_va_arg(void);");
    inject_line(
        "typedef struct {"
        "   unsigned int gp_offset;"
        "   unsigned int fp_offset;"
        "   void *overflow_arg_area;"
        "   void *reg_save_area;"
        "} __builtin_va_list[1];");
}

/*
 * Add default search paths last, with lowest priority. These are
 * searched after anything specified with -I, and in the order listed.
 */
static void add_include_search_paths(void)
{
    add_include_search_path("/usr/local/include");
    add_include_search_path(LACC_STDLIB_PATH);
    add_include_search_path("/usr/include/x86_64-linux-gnu");
    add_include_search_path("/usr/include");
}

int main(int argc, char *argv[])
{
    char *filename;
    struct definition *def;
    const struct symbol *sym;

    filename = parse_program_arguments(argc, argv);
    add_include_search_paths();
    init(filename);
    register_builtin_definitions();
    set_compile_target(output, filename);

    if (context.target == TARGET_NONE) {
        preprocess(output);
    } else {
        push_scope(&ns_ident);
        push_scope(&ns_tag);
        register_builtin_declarations();
        push_optimization(optimization_level);

        while ((def = parse()) != NULL) {
            if (context.errors) {
                error("Aborting because of previous %s.",
                    (context.errors > 1) ? "errors" : "error");
                break;
            }

            optimize(def);
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
        pop_optimization();
        pop_scope(&ns_tag);
        pop_scope(&ns_ident);
    }

    if (output != stdout) {
        fclose(output);
    }

    return context.errors;
}
