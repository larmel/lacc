#ifndef TYPETREE_H
#define TYPETREE_H

#include <stdlib.h>
#include <string.h>

#define BASIC_TYPE_SIGNED(w) \
    ((w) == 1) ? &basic_type__char :                                           \
    ((w) == 2) ? &basic_type__short :                                          \
    ((w) == 4) ? &basic_type__int : &basic_type__long;

#define BASIC_TYPE_UNSIGNED(w) \
    ((w) == 1) ? &basic_type__unsigned_char :                                  \
    ((w) == 2) ? &basic_type__unsigned_short :                                 \
    ((w) == 4) ? &basic_type__unsigned_int : &basic_type__unsigned_long;

/* Reflect semantics given in standardese.
 */
#define is_object(t) (!is_function(t))
#define is_function(t) ((t)->type == T_FUNCTION)
#define is_struct_or_union(t) ((t)->type == T_STRUCT || (t)->type == T_UNION)
#define is_integer(t) ((t)->type == T_SIGNED || is_unsigned(t))
#define is_unsigned(t) ((t)->type == T_UNSIGNED)
#define is_pointer(t) ((t)->type == T_POINTER)
#define is_arithmetic(t) (is_integer(t) || (t)->type == T_REAL)
#define is_scalar(t) (is_arithmetic(t) || (t)->type == T_POINTER)
#define is_aggregate(t) ((t)->type == T_ARRAY || ((t)->type == T_STRUCT))
#define is_void(t) ((t)->type == T_VOID)
#define is_array(t) ((t)->type == T_ARRAY)
#define is_struct(t) ((t)->type == T_STRUCT)
#define is_union(t) ((t)->type == T_UNION)
#define is_const(t) ((t)->qualifier & Q_CONST)
#define is_volatile(t) ((t)->qualifier & Q_VOLATILE)
#define is_tagged(t) (is_struct_or_union(t) && (t)->next)

struct member_list;

/* Internal representation of a type.
 */
struct typetree {
    enum type {
        T_SIGNED,
        T_UNSIGNED,
        T_REAL,
        T_POINTER,
        T_FUNCTION,
        T_ARRAY,
        T_STRUCT,
        T_UNION,
        T_VOID
    } type;

    /* Total storage size in bytes, returned for sizeof. */
    int size;

    enum qualifier {
        Q_NONE = 0,
        Q_CONST = 1,
        Q_VOLATILE = 2,
        Q_CONST_VOLATILE = Q_CONST | Q_VOLATILE
    } qualifier;

    /* Function parameters, or struct/union members. */
    const struct member_list *member_list;

    /* Function return value, pointer target, array base, or pointer to
     * tagged struct or union type. Tag indirections are used to avoid
     * loops in type trees. */
    const struct typetree *next;

    /* Struct or union tag name, taken from symbol table in order to be
     * able to print the reference. */
    const char *tag_name;
};

struct member {
    const char *name;
    const struct typetree *type;
    int offset;
};

/* Get the number of struct or union members, or function parameters.
 */
int nmembers(const struct typetree *type);

/* Return the n-th struct or union member, or function parameter.
 */
const struct member *get_member(const struct typetree *type, int n);

/* Singleton unqualified instances of common types.
 */
extern const struct typetree
    basic_type__void,
    basic_type__const_void,
    basic_type__char,
    basic_type__short,
    basic_type__int,
    basic_type__long,
    basic_type__unsigned_char,
    basic_type__unsigned_short,
    basic_type__unsigned_int,
    basic_type__unsigned_long,
    basic_type__float,
    basic_type__double;

/* A function takes variable arguments if last parameter is '...'.
 */
int is_vararg(const struct typetree *type);

/* Return size of type. If indirection, return size of tagged type.
 */
int size_of(const struct typetree *type);

/* Alignment in bytes.
 */
int type_alignment(const struct typetree *type);

/* Returns 1 if types are equal, 0 otherwise.
 */
int type_equal(const struct typetree *l, const struct typetree *r);

/* Return tagged type if this is an indirection, ignoring cv-qualifiers.
 * The tag is immutable.
 */
const struct typetree *unwrapped(const struct typetree *type);

/* Serialize type to string. Allocates memory with malloc, caller is
 * responsible for calling free.
 */
char *typetostr(const struct typetree *type);

#endif
