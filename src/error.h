#ifndef ERROR_H
#define ERROR_H

#include "input.h"

#include <stddef.h>

extern unsigned errors;

/* Error reporting, can be called from any component */
void error(const char *, ...);

/* Variadic macros not supported in C89, assume always used with one arg. */
#define internal_error(s, m) \
    do { \
        fprintf(stderr, "(%s, %d) internal error in %s, line %d: ", current_file.name, current_file.line, __FILE__, __LINE__); \
        fprintf(stderr, s, m); \
        fprintf(stderr, "\n"); \
    } while (0);

#endif
