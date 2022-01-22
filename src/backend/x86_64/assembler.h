#ifndef ASSEMBLER_H
#define ASSEMBLER_H

#include "encoding.h"
#include <lacc/ir.h>

INTERNAL int assemble_inline(
    struct asm_statement st,
    int (*emit)(struct instruction));

#endif
