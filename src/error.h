#ifndef ERROR_H
#define ERROR_H

#include "input.h"

#include <stddef.h>

extern unsigned errors;

/* Diagnostics info, written to stdout. No-op if -v is not specified.
 */
void verbose(const char *, ...);

/* Error reporting, written to stderr.
 */
void error(const char *, ...);

/* Variadic macros not supported in C89, assume always used with one arg.
 */
#define internal_error(s, m)                                                   \
    do {                                                                       \
        fprintf(stderr, "(%s, %d) internal error in %s, line %d: ",            \
            current_file.name, current_file.line, __FILE__, __LINE__);         \
        fprintf(stderr, s, m);                                                 \
        fprintf(stderr, "\n");                                                 \
    } while (0);

#endif
