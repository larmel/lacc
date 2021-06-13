#ifndef CONTEXT_H
#define CONTEXT_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include <stddef.h>

enum target {
    TARGET_PREPROCESS,
    TARGET_IR_DOT,
    TARGET_x86_64_ASM,
    TARGET_x86_64_OBJ,
    TARGET_x86_64_EXE
};

enum cstd {
    STD_C89,
    STD_C99,
    STD_C11
};

/* Global information about translation unit. */
INTERNAL struct context {
    int errors;
    int verbose;
    int suppress_warning;
    unsigned int pic : 1;            /* position independent code */
    unsigned int debug : 1;          /* Generate debug information. */
    unsigned int no_common : 1;      /* Don't use COMMON symbols. */
    unsigned int no_sse : 1;         /* Don't use SSE instructions. */
    unsigned int pedantic : 1;
    enum target target;
    enum cstd standard;
} context;

/*
 * Output diagnostics info to stdout. No-op if context.verbose is not
 * set.
 */
INTERNAL void verbose(const char *, ...);

/*
 * Output warning to stderr. No-op if context.suppress_warning is set.
 */
INTERNAL void warning(const char *, ...);

/* Output error to stderr. */
INTERNAL void error(const char *, ...);

#endif
