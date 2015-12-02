#ifndef COMPILE_H
#define COMPILE_H

#include "../core/cfg.h"

#include <stdio.h>

enum compile_target {
    TARGET_NONE,
    TARGET_IR_DOT,
    TARGET_x86_64_ASM,
    TARGET_x86_64_ELF
};

/* Initialize compile target format and output stream. Must be called before
 * any compile_* function.
 */
void set_compile_target(FILE *stream, enum compile_target target);

/* Compile control flow graph.
 */
int compile_cfg(struct cfg *cfg);

/* Compile tentative symbols, that have not been assigned a value in this
 * translation unit.
 */
int compile_symbols(struct symbol_list list);

#endif
