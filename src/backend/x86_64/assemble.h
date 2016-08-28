#ifndef ASSEMBLE_H
#define ASSEMBLE_H

#include "instr.h"

#include <stdio.h>

extern FILE *asm_output;

/*
 * Start processing symbol. If the symbol is static, data will follow.
 * If the symbol is of function type, instructions should follow. The
 * end of a symbol context is reached when this function is called
 * again, or on flush.
 */
int asm_symbol(const struct symbol *sym);

/* Add instruction to function context. */
int asm_text(struct instruction instr);

/* Add data to internal symbol context. */
int asm_data(struct immediate data);

/* Write any buffered data to output. */
int asm_flush(void);

#endif
