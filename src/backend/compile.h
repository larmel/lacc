#ifndef COMPILE_H
#define COMPILE_H

#include <lacc/ir.h>

#include <stdio.h>

/*
 * Initialize compile target format and output stream. Must be called
 * before any other compile function.
 */
INTERNAL void set_compile_target(FILE *stream, const char *file);

/* Compile definition, symbols which are assigned some storage. */
INTERNAL int compile(struct definition *def);

/*
 * Compile tentative definitions and declarations; symbols which have
 * not been assigned a value in this translation unit, but must be known
 * for linkage.
 */
INTERNAL int declare(const struct symbol *sym);

/* Flush any buffered output, no more input will follow. */
INTERNAL void flush(void);

/* Free resources after all input objects have been processed. */
INTERNAL void finalize(void);

#endif
