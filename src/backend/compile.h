#ifndef COMPILE_H
#define COMPILE_H

#include "../core/cfg.h"

#include <stdio.h>

enum compile_target {
    TARGET_x86_64_ASM,
    TARGET_x86_64_ELF
};

/* Initialize compile target format and output stream. Must be called before
 * compile().
 */
void set_compile_target(FILE *stream, enum compile_target target);

/* Compile control flow graph.
 */
int compile(struct cfg *cfg);

#endif
