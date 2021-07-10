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
# include "parser/builtin.c"
#else
# define INTERNAL
# define EXTERNAL extern
# include "backend/compile.h"
# include "backend/linker.h"
# include "optimizer/optimize.h"
# include "parser/builtin.h"
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

#if !defined(LIB_PATH) || !defined(INCLUDE_PATHS)
# error Missing required configuration
#endif

static enum lang {
    LANG_UNKNOWN,
    LANG_C,
    LANG_ASM
} source_language;

struct input_file {
    const char *name;
    const char *output_name;
    int is_default_name;
    enum lang language;
};

static const char *program, *output_name;
static int optimization_level;
static int dump_symbols, dump_types;

static array_of(struct input_file) input_files;
static array_of(char *) predefined_macros;
static array_of(const char *) system_include_paths;

static int help(const char *arg)
{
    fprintf(
        stderr,
        "Usage: %s [-(S|E|c)] [-I <path>] [-o <file>] <file ...>\n",
        program);
    return 1;
}

static int version(const char *arg)
{
    fprintf(stdout, "lacc version 0.0.1");
#ifndef NDEBUG
    fprintf(stdout, " DEBUG");
#endif
    fprintf(stdout, "\n");
    return 1;
}

static int flag(const char *arg)
{
    assert(arg[0] == '-');
    assert(strlen(arg) == 2);
    switch (arg[1]) {
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
        } else if (!strcmp("common", arg)) {
            context.no_common = disable;
        } else if (!strcmp("fast-math", arg)) {
            /* Always slow... */
        } else if (!strcmp("strict-aliasing", arg)) {
            /* We don't consider aliasing. */
        } else assert(0);
    } else if (arg[1] == 'm') {
        arg = arg + 2;
        disable = strncmp("no-", arg, 3) == 0;
        if (disable) {
            arg = arg + 3;
        }
        if (!strcmp("sse", arg) || !strcmp("sse2", arg)
            || !strcmp("mmx", arg)
            || !strcmp("3dnow", arg))
        {
            context.no_sse = 1;
        } else assert(0);
    } else if (!strcmp("-dot", arg)) {
        context.target = TARGET_IR_DOT;
    } else if (!strcmp("-nostdinc", arg)) {
        context.nostdinc = 1;
    } else if (!strcmp("-pedantic", arg)) {
        context.pedantic = 1;
    }

    return 0;
}

static int language(const char *arg)
{
    enum lang lang;

    assert(arg);
    if (!strcmp("c", arg)
        || !strcmp("c-header", arg)
        || !strcmp("c-cpp-output", arg))
    {
        lang = LANG_C;
    } else if (!strcmp("assembler", arg)) {
        lang = LANG_ASM;
    } else if (!strcmp("none", arg)) {
        lang = LANG_UNKNOWN;
    } else {
        fprintf(stderr, "Unrecognized input language %s.\n", arg);
        return 1;
    }

    source_language = lang;
    return 0;
}

/* Support -fvisibility, with no effect. */
static int set_visibility(const char *arg)
{
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

static int add_system_include_path(const char *path)
{
    array_push_back(&system_include_paths, path);
    return 0;
}

/*
 * Write to default file if -o, -S or -dot is specified, using input
 * file name with suffix changed to '.o', '.s' or '.dot', respectively.
 *
 * We also need to strip any path information, so that lacc can write
 * its output to the current working directory. This matches what gcc
 * and clang do.
 */
static char *change_file_suffix(const char *file, enum target target)
{
    char *name, *suffix;
    const char *slash, *dot;
    size_t len;

    switch (target) {
    default: assert(0);
    case TARGET_PREPROCESS:
        return NULL;
    case TARGET_IR_DOT:
        suffix = ".dot";
        break;
    case TARGET_x86_64_ASM:
        suffix = ".s";
        break;
    case TARGET_x86_64_OBJ:
    case TARGET_x86_64_EXE:
        suffix = ".o";
        break;
    }

    slash = strrchr(file, '/');
    if (slash) {
        file = slash + 1;
    }

    dot = strrchr(file, '.');
    if (!dot) {
        dot = file + strlen(file);
    }

    len = (dot - file);
    name = calloc(len + strlen(suffix) + 1, sizeof(*name));
    strncpy(name, file, len);
    assert(!name[len] || name[len] == '.');
    name[len] = '.';
    strcpy(name + len, suffix);
    return name;
}

static int add_input_file(const char *name)
{
    char *ptr;
    struct input_file file = {0};

    if ((file.language = source_language) == LANG_UNKNOWN) {
        ptr = strrchr(name, '.');
        if (ptr && (ptr[1] == 'c' || ptr[1] == 'i') && ptr[2] == '\0') {
            file.language = LANG_C;
        }
    }

    file.name = name;
    array_push_back(&input_files, file);

    /*
     * Linker argument might not be needed, but make sure order is
     * preserved.
     */
    if (file.language != LANG_UNKNOWN) {
        ptr = change_file_suffix(name, TARGET_x86_64_OBJ);
        add_linker_arg(ptr);
        free(ptr);
    } else {
        add_linker_arg(name);
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
    if (!strcmp("c89", std) || !strcmp("gnu89", std)) {
        context.standard = STD_C89;
    } else if (!strcmp("c99", std) || !strcmp("gnu99", std)) {
        context.standard = STD_C99;
    } else if (!strcmp("c11", std) || !strcmp("gnu11", std)) {
        context.standard = STD_C11;
    } else {
        fprintf(stderr, "Unrecognized c standard %s.\n", std);
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

/*
 * Print full path of library, or just echo the name if not found.
 *
 * -print-file-name=include can be used to find location of compiler
 * builtin headers.
 */
static int print_file_name(const char *name)
{
    FILE *f;
    char *path;

    assert(name);
    path = calloc(1, strlen(LIB_PATH) + strlen(name) + 2);
    strcpy(path, LIB_PATH);
    strcat(path, "/");
    strcat(path, name);
    if ((f = fopen(path, "r")) != 0) {
        printf("%s\n", path);
        fclose(f);
    } else {
        printf("%s\n", name);
    }

    free(path);
    return -1;
}

static int parse_program_arguments(int argc, char *argv[])
{
    int i, n, k;
    struct input_file *file;
    struct option optv[] = {
        {"-S", &flag},
        {"-E", &flag},
        {"-c", &flag},
        {"-v", &flag},
        {"-w", &flag},
        {"-g", &flag},
        {"-W", &warn},
        {"-W<", &warn},
        {"-x:", &language},
        {"-f[no-]PIC", &option},
        {"-f[no-]fast-math", &option},
        {"-f[no-]strict-aliasing", &option},
        {"-f[no-]common", &option},
        {"-fvisibility=", &set_visibility},
        {"-m[no-]sse", &option},
        {"-m[no-]sse2", &option},
        {"-m[no-]3dnow", &option},
        {"-m[no-]mmx", &option},
        {"-dot", &option},
        {"--help", &help},
        {"--version", &version},
        {"-march=", &set_cpu},
        {"-o:", &set_output_name},
        {"-I:", &add_include_search_path},
        {"-O{0|1|2|3}", &set_optimization_level},
        {"-std=", &set_c_std},
        {"-D:", &define_macro},
        {"--dump-symbols", &long_option},
        {"--dump-types", &long_option},
        {"-nostdinc", &option},
        {"-isystem:", &add_system_include_path},
        {"-include:", &add_include_file},
        {"-print-file-name=", &print_file_name},
        {"-pipe", &option},
        {"-pedantic", &option},
        {"-MD", &option},
        {"-MP", &option},
        {"-Wl,", &add_linker_flag},
        {"-rdynamic", &add_linker_flag},
        {"-shared", &add_linker_arg},
        {"-static", &add_linker_arg},
        {"-[no]pie", &add_linker_arg},
        {"-f[no-]PIE", &add_linker_arg},
        {"-l:", &add_linker_library},
        {"-L:", &add_linker_path},
        {NULL, &add_input_file}
    };

    program = argv[0];
    context.standard = STD_C99;
    context.target = TARGET_x86_64_EXE;
    context.pic = 1;

    if ((i = parse_args(optv, argc, argv)) != 0) {
        return i;
    }

    for (i = 0, k = 0; i < array_len(&input_files); ++i) {
        file = &array_get(&input_files, i);
        if (file->language == LANG_UNKNOWN) {
            switch (context.target) {
            case TARGET_PREPROCESS:
                file->language = LANG_C;
                break;
            case TARGET_x86_64_EXE:
                array_erase(&input_files, i);
                i--;
                k++;
                break;
            default:
                fprintf(stderr, "Unrecognized input file %s.\n", file->name);
                return 1;
            }
        }
    }

    n = array_len(&input_files);
    if (n == 0 && (k == 0 || context.target != TARGET_x86_64_EXE)) {
        fprintf(stderr, "%s\n", "No input files.");
        return 1;
    }

    if (output_name && context.target != TARGET_x86_64_EXE) {
        if (n > 1) {
            fprintf(stderr, "%s\n", "Cannot set -o with multiple inputs.");
            return 1;
        }

        assert(n == 1);
        file = &array_get(&input_files, 0);
        file->output_name = output_name;
    } else for (i = 0; i < n; ++i) {
        file = &array_get(&input_files, i);
        file->output_name = change_file_suffix(file->name, context.target);
        file->is_default_name = 1;
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
 * Add default search paths last, with lowest priority. These are
 * searched after anything specified with -I, and in the order listed.
 */
static void add_include_search_paths(void)
{
    int i;
    const char *path;

    static const char *paths[] = { INCLUDE_PATHS };

    if (!context.nostdinc) {
        for (i = 0; i < sizeof(paths) / sizeof(paths[0]); ++i) {
            add_include_search_path(paths[i]);
        }
    }

    for (i = 0; i < array_len(&system_include_paths); ++i) {
        path = array_get(&system_include_paths, i);
        add_include_search_path(path);
    }

    array_clear(&system_include_paths);
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
        register_builtins();
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
        symtab_clear();
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
    return ret < 0 ? 0 : ret;
}
