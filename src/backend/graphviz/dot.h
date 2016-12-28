#ifndef DOT_H
#define DOT_H

#include <lacc/ir.h>

#include <stdio.h>

/* Set output target. */
void dot_init(FILE *output);

/*
 * Output internal control flow graph intermediate representation in dot
 * format, which can be compiled for rendering.
 */
void dotgen(struct definition *def);

#endif
