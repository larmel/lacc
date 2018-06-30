#ifndef LINKER_H
#define LINKER_H

#include <stdio.h>

/* Call once on startup, with output handle and file name. */
INTERNAL void init_linker(FILE *output, const char *name);

/* Add an option to be passed to the linker. */
INTERNAL void linker_option(const char *opt);

/* Call the linker. */
INTERNAL int linker(void);

#endif
