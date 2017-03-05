#ifndef TYPETREE_H
#define TYPETREE_H

#include <lacc/type.h>

#include <stdlib.h>
#include <string.h>

/*
 * Initialize array, pointer, function, struct or union type.
 *
 * Valid invocations are:
 *
 *   type_create(T_ARRAY, <Type elem>, <size_t length>)
 *   type_create(T_POINTER, <Type next>)
 *   type_create(T_FUNCTION, <Type return>)
 *   type_create(T_STRUCT)
 *   type_create(T_UNION)
 */
Type type_create(enum type, ...);

/* Add const qualifier to type. */
Type type_set_const(Type type);

/* Add volatile qualifier to type. */
Type type_set_volatile(Type type);

/* Set qualifiers from other on type. */
Type type_apply_qualifiers(Type type, Type other);

/*
 * Add member to struct, union or function type, updating size and
 * alignment of members.
 *
 * For functions taking variable number of arguments, the last member
 * should be passed as "...".
 */
void type_add_member(Type parent, String name, Type type, struct symbol *sym);

/*
 * Add unnamed struct or union member, which itself has to be struct or
 * union type.
 *
 * This is a C11 feature.
 */
void type_add_anonymous_member(Type parent, Type type);

/*
 * Add bitfield to struct.
 *
 * Type must be either signed or unsigned int.
 */
void type_add_field(Type parent, String name, Type type, int width);

/*
 * Commit type members and calculate final trailing padding. Must be
 * called after adding struct or union members of a new type.
 */
void type_seal(Type parent);

/*
 * Complete array by setting size, called after reading initializer
 * elements.
 */
void type_set_array_size(Type type, size_t size);

/*
 * Complete declarator by joining target to tail of outer pointer,
 * function, or array type.
 *
 * In declarations like int (*foo)(char), the input will be:
 *
 *    head:   * void
 *    target: (char) -> int
 *
 * The void terminal of head is a placeholder. This function replaces it
 * with target, creating the resulting type:
 *
 *    * (char) -> int
 *
 */
Type type_patch_declarator(Type head, Type target);

/* Specify name for tagged struct or union type. */
void type_set_tag(Type type, String tag);

/*
 * Find type member of the given name, meaning struct or union field, or
 * function parameter. Returns NULL in the case no member is found.
 */
const struct member *find_type_member(Type type, String name);

/* */
int is_compatible(Type l, Type r);

/* Get the type the given pointer is pointing to. */
Type type_deref(Type type);

/*
 * Promote the given integer type to int or unsigned int, or do nothing
 * if the precision is already as wide. For example, unsigned short will
 * be converted to int.
 */
Type promote_integer(Type type);

#endif
