#ifndef TYPETREE_H
#define TYPETREE_H

#ifndef INTERNAL
# define INTERNAL
#endif

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
INTERNAL void clear_types(FILE *stream);

/* Initialize array, pointer, function, struct or union type. */
INTERNAL Type type_create(enum type);
INTERNAL Type type_create_pointer(Type next);
INTERNAL Type type_create_function(Type next);
INTERNAL Type type_create_array(Type next, size_t count);
INTERNAL Type type_create_incomplete(Type next);
INTERNAL Type type_create_vla(Type next, const struct symbol *count);

/* Add const, volatile, and restrict qualifiers to type. */
INTERNAL Type type_set_const(Type type);
INTERNAL Type type_set_volatile(Type type);
INTERNAL Type type_set_restrict(Type type);

/* Get type without any (top level) qualifiers. */
INTERNAL Type type_unqualified(Type type);

/* Set qualifiers from other on type. */
INTERNAL Type type_apply_qualifiers(Type type, Type other);

/* Sentinel type used during construction. */
INTERNAL int is_type_placeholder(Type type);
INTERNAL Type get_type_placeholder(void);

/*
 * Add member to struct, union or function type, updating size and
 * alignment of members.
 *
 * For functions taking variable number of arguments, the last member
 * should be passed as "...".
 */
INTERNAL struct member *type_add_member(Type parent, String name, Type type);

/*
 * Add unnamed struct or union member, which itself has to be struct or
 * union type.
 *
 * This is a C11 feature.
 */
INTERNAL void type_add_anonymous_member(Type parent, Type type);

/*
 * Add bitfield to struct or union.
 *
 * Field type must be either signed or unsigned int.
 */
INTERNAL void type_add_field(Type parent, String name, Type type, size_t width);

/*
 * Commit type members and calculate final trailing padding. Must be
 * called after adding struct or union members of a new type.
 */
INTERNAL void type_seal(Type parent);

/*
 * Complete array type by specifying a length, called after reading
 * initializer elements.
 */
INTERNAL void set_array_length(Type type, size_t length);

/* Number of elements in array. */
INTERNAL size_t type_array_len(Type type);

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
INTERNAL Type type_patch_declarator(Type head, Type target);

/*
 * Remove symbol references from prototype declaration, pointing to
 * function parameters or VLA length constraints that will be invalid
 * beyond prototype scope.
 *
 * For example, a declaration like int foo(int n, int a[][n]) will be
 * converted to int foo(int n, int a[][*]).
 */
INTERNAL void type_clean_prototype(Type type);

/*
 * Specify tag for struct or union type, or typedef. The tag symbol will
 * always be prioritized over typedef.
 */
INTERNAL void type_set_tag(Type type, const struct symbol *tag);

/*
 * Find type member of the given name, meaning struct or union field, or
 * function parameter.
 *
 * Returns NULL in the case no member is found. Index is bound to the
 * numberic index of the member in the member list, or -1 if not found.
 */
INTERNAL const struct member *find_type_member(
	Type type,
	String name,
	int *index);

/* Get symbol holding number of elements in given VLA type. */
INTERNAL const struct symbol *type_vla_length(Type type);

/* */
INTERNAL int is_compatible(Type l, Type r);

/*
 * Determine whether types are compatible if disregarding first level of
 * qualifiers. For example, `int *` and `int * const` are compatible if
 * the const qualifier is ignored.
 */
INTERNAL int is_compatible_unqualified(Type l, Type r);

/* Determine whether a type is VLA, or pointer to VLA. */
INTERNAL int is_variably_modified(Type type);

/* */
INTERNAL int is_complete(Type type);

/* Get the type the given pointer is pointing to. */
INTERNAL Type type_deref(Type type);

/*
 * Promote the given integer type to int or unsigned int, or do nothing
 * if the precision is already as wide. For example, unsigned short will
 * be converted to int.
 */
INTERNAL Type promote_integer(Type type);

/* Apply default argument promotions to type. */
INTERNAL Type default_argument_promotion(Type type);

#endif
