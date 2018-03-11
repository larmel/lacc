#ifndef INITIALIZER_H
#define INITIALIZER_H

#include <lacc/ir.h>

/*
 * Parse and emit code for initializer expressions, such as the right
 * hand side of the following assignments:
 *
 *     int b[] = {0, 1, 2, 3};
 *     floaf f = 3.14f;
 *
 * Generates a series of assignment operations on references to target
 * symbol, with increasing offsets.
 *
 * An initializer can either be an assignment expression, or a brace-
 * enclosed initializer list.
 */
INTERNAL struct block *initializer(
    struct definition *def,
    struct block *block,
    const struct symbol *sym);

#endif
