#ifndef ASSEMBLER_H
#define ASSEMBLER_H

#include "x86_64/instr.h"
#include <lacc/ir.h>

INTERNAL int assemble_inline(
    struct asm_statement st,
    int (*emit)(struct instruction));

#endif
