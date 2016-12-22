#ifndef TYPE_H
#define TYPE_H

#include <lacc/typetree.h>

#include <stdlib.h>
#include <string.h>

/*
 * Add member to struct, union or function type. Update size and
 * alignment of members. For functions taking variable number of
 * arguments, the last member should be passed as "...".
 */
void type_add_member(
    struct typetree *parent,
    String name,
    const struct typetree *type);

/*
 * Add unnamed struct or union member, which itself has to be struct or
 * union type.
 *
 * This is a C11 feature.
 */
void type_add_anonymous_member(
    struct typetree *parent,
    const struct typetree *type);

/*
 * Add bitfield to struct. Type must be either signed or unsigned int.
 * There is no packing; fields are always stored as separate integers,
 * occupying 4 bytes each.
 */
void type_add_field(
    struct typetree *parent,
    String name,
    const struct typetree *type,
    int width);

/*
 * Commit type members and calculate final trailing padding. Must be
 * called after adding struct or union members of a new type.
 */
void type_seal(struct typetree *parent);

/*
 * Find type member of the given name, meaning struct or union field, or
 * function parameter. Returns NULL in the case no member is found.
 */
const struct member *find_type_member(
    const struct typetree *type,
    String name);

/*
 * Allocate and initialize a new type. Take additional parameters for
 * initializing integer, pointer and array types, otherwise all-zero
 * (default) values.
 *
 *      type_init(T_SIGNED, [size])
 *      type_init(T_POINTER, [next])
 *      type_init(T_ARRAY, [next], [count])
 */
struct typetree *type_init(enum type tt, ...);

/*
 * Create a tag type pointing to the provided object. Input must be
 * of struct or union type.
 *
 * Usage of this is to avoid circular typetree graphs, and to let tagged
 * types be cv-qualified without mutating the original definition.
 */
struct typetree *type_tagged_copy(
    const struct typetree *type,
    String name);

int is_compatible(const struct typetree *l, const struct typetree *r);

/*
 * Return a copy of type at root level, leaving all members, return, and
 * pointer type unchanged.
 */
struct typetree *type_shallow_copy(const struct typetree *type);

/*
 * Get the type the given POINTER is pointing to. Handles tag
 * indirections for pointers to typedef'ed object types.
 */
const struct typetree *type_deref(const struct typetree *type);

/*
 * Find a common real type between operands used in an expression,
 * giving the type of the result.
 */
const struct typetree *usual_arithmetic_conversion(
    const struct typetree *t1,
    const struct typetree *t2);

/*
 * Promote the given integer type to int or unsigned int, or do nothing
 * if the precision is already as wide. For example, unsigned short will
 * be converted to int.
 */
const struct typetree *promote_integer(const struct typetree *type);

#endif
