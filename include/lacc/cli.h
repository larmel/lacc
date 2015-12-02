#ifndef CLI_H
#define CLI_H

#include <stddef.h>

extern unsigned errors;
extern int verbose_level;

/* Diagnostics info, written to stdout. No-op if verbose_level is zero.
 */
void verbose(const char *, ...);

/* Error reporting, written to stderr.
 */
void error(const char *, ...);

#endif
