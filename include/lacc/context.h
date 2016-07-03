#ifndef CONTEXT_H
#define CONTEXT_H

#include <stddef.h>

/* Global information about translation unit. Accessible from all
 * modules.
 */
extern struct context {
    int errors;
    int verbose;
    enum target {
        TARGET_NONE,
        TARGET_IR_DOT,
        TARGET_x86_64_ASM,
        TARGET_x86_64_ELF
    } target;
    enum {
        STD_C89
    } standard;
} context;

/* Diagnostics info, written to stdout. No-op if context.verbose is
 * not set.
 */
void verbose(const char *, ...);

/* Error reporting, written to stderr.
 */
void error(const char *, ...);

#endif
