#ifndef TYPETREE_H
#define TYPETREE_H

#include <lacc/type.h>

#include <stdlib.h>
#include <string.h>

/*
 * Deallocate memory used to store types. If not NULL, output types to
 * stream. 
 *
 * This must be called once at the end of the translation unit, and
 * before symbols are deallocated (since types can refer to tags).
 */
void clear_types(FILE *stream);

/* Initialize array, pointer, function, struct or union type. */
Type type_create(enum type);
Type type_create_pointer(Type next);
Type type_create_function(Type next);
Type type_create_array(Type next, size_t count);
Type type_create_incomplete(Type next);
Type type_create_vla(Type next, const struct symbol *count);

/* Add const, volatile, and restrict qualifiers to type. */
Type type_set_const(Type type);
Type type_set_volatile(Type type);
Type type_set_restrict(Type type);

/* Set qualifiers from other on type. */
Type type_apply_qualifiers(Type type, Type other);

/*
 * Add member to struct, union or function type, updating size and
 * alignment of members.
 *
 * For functions taking variable number of arguments, the last member
 * should be passed as "...".
 */
struct member *type_add_member(Type parent, String name, Type type);

/*
 * Add unnamed struct or union member, which itself has to be struct or
 * union type.
 *
 * This is a C11 feature.
 */
void type_add_anonymous_member(Type parent, Type type);

/*
 * Add bitfield to struct or union.
 *
 * Field type must be either signed or unsigned int.
 */
void type_add_field(Type parent, String name, Type type, size_t width);

/*
 * Commit type members and calculate final trailing padding. Must be
 * called after adding struct or union members of a new type.
 */
void type_seal(Type parent);

/*
 * Complete array type by specifying a length, called after reading
 * initializer elements.
 */
void set_array_length(Type type, size_t length);

/* Number of elements in array. */
size_t type_array_len(Type type);

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

/*
 * Remove symbol references from prototype declaration, pointing to
 * function parameters or VLA length constraints that will be invalid
 * beyond prototype scope.
 *
 * For example, a declaration like int foo(int n, int a[][n]) will be
 * converted to int foo(int n, int a[][*]).
 */
void type_clean_prototype(Type type);

/*
 * Specify tag for struct or union type, or typedef. The tag symbol will
 * always be prioritized over typedef.
 */
void type_set_tag(Type type, const struct symbol *tag);

/*
 * Find type member of the given name, meaning struct or union field, or
 * function parameter.
 *
 * Returns NULL in the case no member is found. Index is bound to the
 * numberic index of the member in the member list, or -1 if not found.
 */
const struct member *find_type_member(Type type, String name, int *index);

/* Get symbol holding number of elements in given VLA type. */
const struct symbol *type_vla_length(Type type);

/* */
int is_compatible(Type l, Type r);

/*
 * Determine whether types are compatible if disregarding first level of
 * qualifiers. For example, `int *` and `int * const` are compatible if
 * the const qualifier is ignored.
 */
int is_compatible_unqualified(Type l, Type r);

/* Determine whether a type is VLA, or pointer to VLA. */
int is_variably_modified(Type type);

/* */
int is_complete(Type type);

/* Get the type the given pointer is pointing to. */
Type type_deref(Type type);

/*
 * Promote the given integer type to int or unsigned int, or do nothing
 * if the precision is already as wide. For example, unsigned short will
 * be converted to int.
 */
Type promote_integer(Type type);

#endif
