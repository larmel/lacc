#if AMALGAMATION
# define INTERNAL static
# define EXTERNAL static
# include "context.c"
# include "util/argparse.c"
# include "util/hash.c"
# include "util/string.c"
# include "backend/x86_64/encoding.c"
# include "backend/x86_64/dwarf.c"
# include "backend/x86_64/elf.c"
# include "backend/x86_64/abi.c"
# include "backend/x86_64/assemble.c"
# include "backend/assembler.c"
# include "backend/compile.c"
# include "backend/graphviz/dot.c"
# include "backend/linker.c"
# include "optimizer/transform.c"
# include "optimizer/liveness.c"
# include "optimizer/optimize.c"
# include "preprocessor/tokenize.c"
# include "preprocessor/strtab.c"
# include "preprocessor/input.c"
# include "preprocessor/directive.c"
# include "preprocessor/preprocess.c"
# include "preprocessor/macro.c"
# include "parser/typetree.c"
# include "parser/symtab.c"
# include "parser/parse.c"
# include "parser/statement.c"
# include "parser/initializer.c"
# include "parser/expression.c"
# include "parser/declaration.c"
# include "parser/eval.c"
#else
# define INTERNAL
# define EXTERNAL extern
# include "backend/compile.h"
# include "backend/linker.h"
# include "optimizer/optimize.h"
# include "parser/parse.h"
# include "parser/symtab.h"
# include "parser/typetree.h"
# include "preprocessor/preprocess.h"
# include "preprocessor/input.h"
# include "preprocessor/macro.h"
# include "util/argparse.h"
# include <lacc/context.h>
# include <lacc/ir.h>
#endif

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

/*
 * Configurable location of system headers. Default on Linux is GNU
 * libc. Can be overridden to point to for example musl.
 *
 * OpenBSD does not need a special path.
 */
#ifndef SYSTEM_STDLIB_PATH
# ifdef __linux__
#  define SYSTEM_STDLIB_PATH "/usr/include/x86_64-linux-gnu"
# endif
#endif

struct input_file {
    const char *name;
    const char *output_name;
    int is_default_name;
};

static const char *program, *output_name;
static int optimization_level;
static int dump_symbols, dump_types;

static int object_file_count;
static array_of(struct input_file) input_files;
static array_of(char *) predefined_macros;

static int help(const char *arg)
{
    fprintf(
        stderr,
        "Usage: %s [-(S|E|c)] [-v] [-fPIC] [-I <path>] [-o <file>] <file ...>\n",
        program);
    return 1;
}

static int flag(const char *arg)
{
    switch (*arg) {
    case 'c':
        context.target = TARGET_x86_64_OBJ;
        break;
    case 'S':
        context.target = TARGET_x86_64_ASM;
        break;
    case 'E':
        context.target = TARGET_PREPROCESS;
        break;
    case 'v':
        context.verbose += 1;
        break;
    case 'g':
        context.debug = 1;
        break;
    case 'w':
        context.suppress_warning = 1;
        break;
    default:
        assert(0);
        break;
    }

    return 0;
}

static int option(const char *arg)
{
    int disable;

    assert(*arg == '-');
    if (arg[1] == 'f') {
        arg = arg + 2;
        disable = strncmp("no-", arg, 3) == 0;
        if (disable) {
            arg = arg + 3;
        }
        if (!strcmp("PIC", arg)) {
            context.pic = !disable;
        } else if (!strcmp("fast-math", arg)) {
            /* Always slow... */
        } else if (!strcmp("strict-aliasing", arg)) {
            /* We don't consider aliasing. */
        } else assert(0);
    } else if (!strcmp("-dot", arg)) {
        context.target = TARGET_IR_DOT;
    }

    return 0;
}

/* Accept anything for -march. */
static int set_cpu(const char *arg)
{
    return 0;
}

/* Ignore all warning options specified with -W<option> */
static int warn(const char *arg)
{
    return 0;
}

static int set_output_name(const char *file)
{
    output_name = file;
    add_linker_arg("-o");
    add_linker_arg(file);
    return 0;
}

/*
 * Write to default file if -o, -S or -dot is specified, using input
 * file name with suffix changed to '.o', '.s' or '.dot', respectively.
 */
static char *change_file_suffix(const char *file, enum target target)
{
    char *name, *suffix;
    const char *dot;
    size_t len;

    switch (target) {
    default: assert(0);
    case TARGET_PREPROCESS:
        return NULL;
    case TARGET_IR_DOT:
        suffix = "dot";
        break;
    case TARGET_x86_64_ASM:
        suffix = "s";
        break;
    case TARGET_x86_64_OBJ:
    case TARGET_x86_64_EXE:
        suffix = "o";
        break;
    }

    dot = strrchr(file, '.');
    if (!dot) {
        dot = file + strlen(file);
    }

    len = (dot - file) + 1;
    name = calloc(len + strlen(suffix) + 1, sizeof(*name));
    strncpy(name, file, len);
    assert(name[len - 1] == '.');
    strcpy(name + len, suffix);
    return name;
}

static int add_input_file(const char *name)
{
    char *ptr, *obj;
    struct input_file file = {0};

    ptr = strrchr(name, '.');
    if (!ptr) {
        fprintf(stderr, "Unrecognized input file '%s'\n", name);
        return 1;
    }

    object_file_count++;
    switch (*(ptr + 1)) {
    case 'c':
    case 'i':
        file.name = name;
        array_push_back(&input_files, file);
        obj = change_file_suffix(name, TARGET_x86_64_OBJ);
        add_linker_arg(obj);
        free(obj);
        break;
    default:
        add_linker_arg(name);
        break;
    }

    return 0;
}

static void clear_input_files(void)
{
    int i;
    struct input_file *file;

    for (i = 0; i < array_len(&input_files); ++i) {
        file = &array_get(&input_files, i);
        if (file->is_default_name) {
            free((void *) file->output_name);
        }
    }

    array_clear(&input_files);
}

static int set_c_std(const char *std)
{
    if (!strcmp("c89", std)) {
        context.standard = STD_C89;
    } else if (!strcmp("c99", std)) {
        context.standard = STD_C99;
    } else if (!strcmp("c11", std)) {
        context.standard = STD_C11;
    } else {
        fprintf(stderr, "Unrecognized option %s.\n", std);
        return 1;
    }

    return 0;
}

static int set_optimization_level(const char *level)
{
    assert(isdigit(level[2]));
    optimization_level = level[2] - '0';
    return 0;
}

static int long_option(const char *arg)
{
    if (!strcmp("--dump-symbols", arg)) {
        dump_symbols = 1;
    } else if (!strcmp("--dump-types", arg)) {
        dump_types = 1;
    }

    return 0;
}

static int define_macro(const char *arg)
{
    char *buf, *ptr;
    size_t len;

    len = strlen(arg) + 11;
    buf = calloc(len, sizeof(*buf));
    ptr = strchr(arg, '=');
    if (ptr) {
        sprintf(buf, "#define %s", arg);
        *(buf + 8 + (ptr - arg)) = ' ';
    } else {
        sprintf(buf, "#define %s 1", arg);
    }

    array_push_back(&predefined_macros, buf);
    return 0;
}

static void clear_predefined_macros(void)
{
    int i;
    char *buf;

    for (i = 0; i < array_len(&predefined_macros); ++i) {
        buf = array_get(&predefined_macros, i);
        free(buf);
    }

    array_clear(&predefined_macros);
}

static int add_linker_flag(const char *arg)
{
    char *end;

    if (!strcmp("-rdynamic", arg)) {
        add_linker_arg("-export-dynamic");
    } else {
        assert(!strncmp("-Wl,", arg, 4));
        end = strchr(arg, ',');
        do {
            arg = end + 1;
            end = strchr(arg, ',');
            if (end) {
                *end = '\0';
            }

            add_linker_arg(arg);
        } while (end);
    }

    return 0;
}

static int add_linker_library(const char *lib)
{
    if (lib[-2] == '-') {
        add_linker_arg(lib - 2);
    } else {
        add_linker_arg("-l");
        add_linker_arg(lib);
    }

    return 0;
}

static int add_linker_path(const char *path)
{
    if (path[-2] == '-') {
        add_linker_arg(path - 2);
    } else {
        add_linker_arg("-L");
        add_linker_arg(path);
    }

    return 0;
}

static int parse_program_arguments(int argc, char *argv[])
{
    int i, input_file_count;
    struct input_file *file;
    struct option optv[] = {
        {"-S", &flag},
        {"-E", &flag},
        {"-c", &flag},
        {"-v", &flag},
        {"-w", &flag},
        {"-g", &flag},
        {"-W<", &warn},
        {"-f[no-]PIC", &option},
        {"-f[no-]fast-math", &option},
        {"-f[no-]strict-aliasing", &option},
        {"-dot", &option},
        {"--help", &help},
        {"-march=", &set_cpu},
        {"-o:", &set_output_name},
        {"-I:", &add_include_search_path},
        {"-O{0|1|2|3}", &set_optimization_level},
        {"-std=", &set_c_std},
        {"-D:", &define_macro},
        {"--dump-symbols", &long_option},
        {"--dump-types", &long_option},
        {"-pipe", &option},
        {"-Wl,", &add_linker_flag},
        {"-rdynamic", &add_linker_flag},
        {"-shared", &add_linker_arg},
        {"-l:", &add_linker_library},
        {"-L:", &add_linker_path},
        {NULL, &add_input_file}
    };

    program = argv[0];
    context.standard = STD_C99;
    context.target = TARGET_x86_64_EXE;

    /* OpenBSD defaults to -fPIC unless explicitly turned off.  */
#ifdef __OpenBSD__
    context.pic = 1;
#endif

    if ((i = parse_args(optv, argc, argv)) != 0) {
        return i;
    }

    input_file_count = array_len(&input_files);
    if (context.target != TARGET_x86_64_EXE
        && input_file_count != object_file_count)
    {
        fprintf(stderr, "%s\n", "Unrecognized input files.");
        return 1;
    }

    if (input_file_count == 0) {
        if (context.target != TARGET_x86_64_EXE || !object_file_count) {
            fprintf(stderr, "%s\n", "No input files.");
            return 1;
        }
    } else if (input_file_count == 1) {
        file = &array_get(&input_files, 0);
        if (output_name && context.target != TARGET_x86_64_EXE) {
            file->output_name = output_name;
        } else {
            file->output_name = change_file_suffix(file->name, context.target);
            file->is_default_name = 1;
        }
    } else {
        if (output_name && context.target != TARGET_x86_64_EXE) {
            fprintf(stderr, "%s\n",
                "Cannot set -o with multiple inputs and -c, -S, -E or -dot.");
            return 1;
        }
        for (i = 0; i < input_file_count; ++i) {
            file = &array_get(&input_files, i);
            file->output_name = change_file_suffix(file->name, context.target);
            file->is_default_name = 1;
        }
    }

    return 0;
}

static void register_argument_definitions(void)
{
    int i;
    char *line;

    for (i = 0; i < array_len(&predefined_macros); ++i) {
        line = array_get(&predefined_macros, i);
        inject_line(line);
    }
}

/*
 * Register compiler internal builtin symbols, that are assumed to
 * exists by standard library headers.
 */
static void register_builtin_declarations(void)
{
    inject_line("void *memcpy(void *dest, const void *src, unsigned long n);");
    inject_line("void __builtin_alloca(unsigned long);");
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
#ifdef SYSTEM_STDLIB_PATH
    add_include_search_path(SYSTEM_STDLIB_PATH);
#endif
    add_include_search_path("/usr/include");
}

static int process_file(struct input_file file)
{
    FILE *output;
    struct definition *def;
    const struct symbol *sym;

    preprocess_reset();
    set_input_file(file.name);
    register_builtin_definitions(context.standard);
    register_argument_definitions();
    if (file.output_name) {
        output = fopen(file.output_name, "w");
        if (!output) {
            fprintf(stderr, "Could not open output file '%s'.\n",
                file.output_name);
            return 1;
        }
    } else {
        output = stdout;
    }

    if (context.target == TARGET_PREPROCESS) {
        preprocess(output);
    } else {
        set_compile_target(output, file.name);
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

        if (dump_symbols) {
            output_symbols(stdout, &ns_ident);
            output_symbols(stdout, &ns_tag);
        }

        flush();
        pop_optimization();
        clear_types(dump_types ? stdout : NULL);
        pop_scope(&ns_tag);
        pop_scope(&ns_ident);
    }

    if (output != stdout) {
        fclose(output);
    }

    return context.errors;
}

int main(int argc, char *argv[])
{
    int i, ret;
    struct input_file file;

    if ((ret = parse_program_arguments(argc, argv)) != 0) {
        goto end;
    }

    add_include_search_paths();
    for (i = 0, ret = 0; i < array_len(&input_files); ++i) {
        file = array_get(&input_files, i);
        if ((ret = process_file(file)) != 0) {
            goto end;
        }
    }

    if (context.target == TARGET_x86_64_EXE) {
        ret = invoke_linker();
    }

end:
    finalize();
    parse_finalize();
    preprocess_finalize();
    clear_predefined_macros();
    clear_input_files();
    clear_linker_args();
    return ret;
}
