#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "linker.h"

#include <sys/wait.h>

#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

static FILE *ld_output;
static const char *ld_name;

struct ld_args {
    char *arg;
    struct ld_args *next;
};
static struct ld_args *ld_head;

INTERNAL void init_linker(FILE *output, const char *name)
{
    char buf[PATH_MAX];

    ld_output = output;
    if (output == stdout)
        ld_name = "a.out";
    else
        ld_name = name;

    strncpy(buf, "/usr/bin/ld", PATH_MAX);

    if ((ld_head = malloc(sizeof(struct ld_args))) == NULL) {
        error("Could not allocate ld argument list.");
        exit(1);
    }
    ld_head->arg = strdup("/usr/bin/ld");
    ld_head->next = NULL;

    /* Is there a way to generalize? */
#ifdef __OpenBSD__
    linker_option("-e");
    linker_option("__start");
    linker_option("--eh-frame-hdr");
    linker_option("-Bdynamic");
    linker_option("-dynamic-linker");
    linker_option("/usr/libexec/ld.so");
    linker_option("-o");
    linker_option(ld_name);
    linker_option("/usr/lib/crt0.o");
    linker_option("/usr/lib/crtbegin.o");
    linker_option("-L/usr/local/lib");
    linker_option("-L/usr/lib");
    linker_option("-lc");
    linker_option("/usr/lib/crtend.o");
#endif
}

INTERNAL void linker_option(const char *opt)
{
    struct ld_args *curr;

    curr = ld_head;
    while (curr->next != NULL)
        curr = curr->next;

    if ((curr->next = malloc(sizeof(struct ld_args))) == NULL) {
        error("Could not allocate ld argument.");
        exit(1);
    }

    curr = curr->next;
    curr->arg = strdup(opt);
    curr->next = NULL;
}

INTERNAL int linker(void)
{
    struct ld_args *curr;
    char **ld_argv, **temp_argv;
    int ld_argc = 0, ret, status;
    pid_t pid;

    curr = ld_head;
    while (curr != NULL) {
        ++ld_argc;
        curr = curr->next;
    }

    if ((ld_argv = malloc((ld_argc + 1) * sizeof(char **))) == NULL) {
        error("Could not build ld command.");
        exit(1);
    }
    temp_argv = ld_argv;

    curr = ld_head;
    while (curr != NULL) {
        *temp_argv++ = strdup(curr->arg);
        curr = curr->next;
    }
    *temp_argv = NULL;

    switch ((pid = fork())) {
    case -1:
        error("ld command failed.");
        exit(1);
    case 0:
        execvp(ld_argv[0], ld_argv);
        _exit(0);
    default:
        waitpid(pid, &status, 0);
        ret = WEXITSTATUS(status);
    }

    return ret;
}
