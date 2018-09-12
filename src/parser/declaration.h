#ifndef DECLARATION_H
#define DECLARATION_H

#include <lacc/ir.h>

INTERNAL struct block *declaration(
    struct definition *def,
    struct block *parent);

/*
 * Parse a declarator. Set name = NULL for abstract declarator, only
 * expecting a type.
 *
 * Declarators can produce evaluation through VLA types.
 */
INTERNAL struct block *declarator(
    struct definition *def,
    struct block *parent,
    Type base,
    Type *type,
    String *name);

INTERNAL Type declaration_specifiers(
    int *storage_class,
    int *is_inline,
    int *is_register);

INTERNAL struct block *declare_vla(
    struct definition *def,
    struct block *block,
    struct symbol *sym);

#define FIRST_type_qualifier \
    CONST: case VOLATILE

#define FIRST_type_specifier \
    VOID: case BOOL: case CHAR: case SHORT: case INT: case LONG: case FLOAT: \
    case DOUBLE: case SIGNED: case UNSIGNED: case STRUCT: case UNION: case ENUM

#define FIRST_type_name \
    FIRST_type_qualifier: \
    case FIRST_type_specifier

#define FIRST(s) FIRST_ ## s

#endif
