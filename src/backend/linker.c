#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "linker.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>

typedef array_of(char *) ArgArray;

static ArgArray ld_args, ld_user_args;
static int is_shared, is_static;

static void add_option(ArgArray *args, const char *opt)
{
    size_t len;
    char *buf;

    len = strlen(opt) + 1;
    buf = malloc(len);
    strcpy(buf, opt);
    array_push_back(args, buf);
}

INTERNAL void clear_linker_args(void)
{
    int i;
    char *ptr;

    array_concat(&ld_args, &ld_user_args);
    array_clear(&ld_user_args);
    for (i = 0; i < array_len(&ld_args); ++i) {
        ptr = array_get(&ld_args, i);
        free(ptr);
    }

    array_clear(&ld_args);
}

static void init_linker(void)
{
    add_option(&ld_args, "/usr/bin/ld");
#if __OpenBSD__
    if (!is_shared) {
        add_option(&ld_args, "-e");
        add_option(&ld_args, "__start");
        add_option(&ld_args, "-dynamic-linker");
        add_option(&ld_args, "/usr/libexec/ld.so");
        if (is_static) {
            add_option(&ld_args, "/usr/lib/rcrt0.o");
        } else {
            add_option(&ld_args, "/usr/lib/crt0.o");
        }
    }

    add_option(&ld_args, "/usr/lib/crtbegin.o");
#elif GLIBC
    if (!is_shared) {
        add_option(&ld_args, "-e");
        add_option(&ld_args, "_start");
        add_option(&ld_args, "-dynamic-linker");
        add_option(&ld_args, "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2");
        if (context.pic) {
            add_option(&ld_args, "/usr/lib/x86_64-linux-gnu/Scrt1.o");
        } else {
            add_option(&ld_args, "/usr/lib/x86_64-linux-gnu/crt1.o");
        }
    }

    add_option(&ld_args, "/usr/lib/x86_64-linux-gnu/crti.o");
    add_option(&ld_args, "-L/usr/lib/x86_64-linux-gnu");
#elif MUSL
    if (!is_shared) {
        add_option(&ld_args, "-e");
        add_option(&ld_args, "_start");
        add_option(&ld_args, "-dynamic-linker");
        add_option(&ld_args, "/lib/ld-musl-x86_64.so.1");
        if (context.pic) {
            add_option(&ld_args, "/usr/lib/Scrt1.o");
        } else {
            add_option(&ld_args, "/usr/lib/crt1.o");
        }
    }

    add_option(&ld_args, "/usr/lib/crti.o");
#endif
    add_option(&ld_args, "-L/usr/local/lib");
    add_option(&ld_args, "-L/usr/lib");
}

INTERNAL int add_linker_arg(const char *opt)
{
    if (!strcmp("-shared", opt)) {
        is_shared = 1;
    } else if (!strcmp("-static", opt)) {
        is_static = 1;
    }

    add_option(&ld_user_args, opt);
    return 0;
}

#ifndef NDEBUG
static void print_invocation(void)
{
    int i;
    char *arg;

    for (i = 0; i < array_len(&ld_args); ++i) {
        arg = array_get(&ld_args, i);
        printf("%s ", arg);
    }

    printf("\n");
}
#endif

INTERNAL int invoke_linker(void)
{
    char **argv;
    int status, ret;
    pid_t pid;

    init_linker();
    array_concat(&ld_args, &ld_user_args);

    add_option(&ld_args, "-lc");
#if __OpenBSD__
    add_option(&ld_args, "/usr/lib/crtend.o");
#elif GLIBC
    add_option(&ld_args, "/usr/lib/x86_64-linux-gnu/crtn.o");
#elif MUSL
    add_option(&ld_args, "/usr/lib/crtn.o");
#endif

#ifndef NDEBUG
    print_invocation();
#endif

    array_push_back(&ld_args, (char *) NULL);
    argv = &array_get(&ld_args, 0);
    switch ((pid = fork())) {
    case 0:
        execvp(argv[0], argv);
        _exit(0);
    case -1:
        fprintf(stderr, "%s\n", "Failed to start linker process.");
        ret = 1;
        break;
    default:
        waitpid(pid, &status, 0);
        ret = WEXITSTATUS(status);
        break;
    }

    array_clear(&ld_user_args);
    return ret;
}
