#ifndef INITIALIZER_H
#define INITIALIZER_H

#include <lacc/ir.h>

/*
 * Parse and emit initializer code for target variable in statements
 * such as int b[] = {0, 1, 2, 3}. Generates a series of assignment
 * operations on references to target variable, with increasing offsets.
 *
 * An initializer can either be an assignment expression, or a brace-
 * enclosed initializer list.
 */
struct block *initializer(
    struct definition *def,
    struct block *block,
    struct var target);

#endif
