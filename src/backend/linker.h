#ifndef LINKER_H
#define LINKER_H

#include <stdio.h>

/* Add command line argument to be passed to the linker. */
INTERNAL int add_linker_arg(const char *opt);

/* Invoke the system linker. */
INTERNAL int invoke_linker(void);

/* Free memory used for linker arguments. */
INTERNAL void clear_linker_args(void);

#endif
