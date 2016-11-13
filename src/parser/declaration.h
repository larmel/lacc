#ifndef DECLARATION_H
#define DECLARATION_H

#include <lacc/ir.h>

struct block *declaration(struct definition *def, struct block *parent);

const struct typetree *declarator(const struct typetree *base, String *name);

const struct typetree *declaration_specifiers(int *stc);

#define FIRST_type_qualifier \
    CONST: case VOLATILE

#define FIRST_type_specifier \
    VOID: case CHAR: case SHORT: case INT: case LONG: case FLOAT: case DOUBLE: \
    case SIGNED: case UNSIGNED: case STRUCT: case UNION: case ENUM

#define FIRST_type_name \
    FIRST_type_qualifier: \
    case FIRST_type_specifier

#define FIRST(s) FIRST_ ## s

#endif
