#ifndef ASSEMBLE_H
#define ASSEMBLE_H

#include "instr.h"

#include <stdio.h>

/* Call once on startup, with output handle and source filename. */
INTERNAL void asm_init(FILE *output, const char *file);

/*
 * Start processing symbol. If the symbol is static, data will follow.
 * If the symbol is of function type, instructions should follow. The
 * end of a symbol context is reached when this function is called
 * again, or on flush.
 */
INTERNAL int asm_symbol(const struct symbol *sym);

/* Add instruction to function context. */
INTERNAL int asm_text(struct instruction instr);

/* Add data to internal symbol context. */
INTERNAL int asm_data(struct immediate data);

/* Write any buffered data to output. */
INTERNAL int asm_flush(void);

#endif
