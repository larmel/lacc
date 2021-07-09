#ifndef BUILTIN_H
#define BUILTIN_H

#include <lacc/ir.h>

/*
 * Register compiler internal builtin symbols, that are assumed to
 * exists by standard library headers.
 */
INTERNAL void register_builtins(void);

#endif
