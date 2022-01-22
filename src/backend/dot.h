#ifndef DOT_H
#define DOT_H

#include <lacc/ir.h>

#include <stdio.h>

/*
 * Output internal control flow graph intermediate representation in dot
 * format, which can be compiled for rendering.
 */
INTERNAL void dotgen(FILE *stream, struct definition *def);

#endif
