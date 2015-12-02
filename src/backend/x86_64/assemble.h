#ifndef ASSEMBLE_H
#define ASSEMBLE_H

#include "instructions.h"

#include <stdio.h>

extern FILE *asm_output;

int asm_enter_context(const struct symbol *sym);

int asm_text(struct instruction instr);

int asm_data(struct immediate data);

int asm_exit_context(void);

#endif
