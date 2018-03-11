#ifndef DIRECTIVE_H
#define DIRECTIVE_H

#include "macro.h"
#include <lacc/token.h>

EXTERNAL struct token
    ident__include,
    ident__defined,
    ident__define,
    ident__ifndef,
    ident__ifdef,
    ident__undef,
    ident__elif,
    ident__endif,
    ident__error,
    ident__pragma,
    ident__Pragma;

/*
 * Preprocess a line starting with a '#' directive. Borrows ownership of
 * input. Assume input is END terminated.
 */
INTERNAL void preprocess_directive(TokenArray *line);

/* Non-zero iff currently not inside a false #if directive. */
INTERNAL int in_active_block(void);

#endif
