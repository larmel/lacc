#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <lacc/ir.h>

INTERNAL struct block *expression(struct definition *def, struct block *block);

INTERNAL struct var constant_expression(void);

INTERNAL struct block *assignment_expression(
	struct definition *def,
	struct block *block);

INTERNAL struct block *conditional_expression(
    struct definition *def,
    struct block *block);

INTERNAL struct block *parse__builtin_va_start(
    struct definition *def,
    struct block *block);

INTERNAL struct block *parse__builtin_va_arg(
    struct definition *def,
    struct block *block);

INTERNAL struct block *parse__builtin_alloca(
    struct definition *def,
    struct block *block);

INTERNAL void expression_parse_finalize(void);

#endif
