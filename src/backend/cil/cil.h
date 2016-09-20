#ifndef CLI_H
#define CLI_H

#include <lacc/ir.h>
#include <lacc/symbol.h>

#include <stdio.h>

void cil_set_output(FILE *stream);

void cil_compile_definition(struct definition *def);

void cil_declare_symbol(const struct symbol *sym);

void cil_flush(void);


#endif
