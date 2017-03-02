#ifndef DECLARATION_H
#define DECLARATION_H

#include <lacc/ir.h>

struct block *declaration(struct definition *def, struct block *parent);

/*
 * Parse a declarator. Set name = NULL for abstract declarator, only
 * expecting a type.
 *
 * Declarators can produce evaluation through VLA types.
 */
struct block *declarator(
    struct definition *def,
    struct block *parent,
    Type base,
    Type *type,
    String *name);

Type declaration_specifiers(int *storage_class, int *is_inline);

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
