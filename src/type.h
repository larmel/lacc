#ifndef TYPE_H
#define TYPE_H

#include <stdlib.h>

/* Internal representation of a type.
 */
struct typetree
{
    enum {
        INTEGER,
        REAL,
        POINTER,
        FUNCTION,
        ARRAY,
        OBJECT,
        NONE
    } type;

    /* Total storage size in bytes, returned for sizeof */
    int size;

    /* Bitfield representing const and volatile qualifiers. */
    unsigned short qualifier;

    /* Bitfield representing union, vararg, and unsigned. */
    unsigned short flags;

    /* Number of function parameters or object members */
    int n;

    /* Function parameters or struct/union members. */
    struct member {
        const struct typetree *type;
        const char *name;
        int offset;
    } *member;

    /* Function return value, pointer target, array base, or pointer to tagged
     * struct or union type. */
    const struct typetree *next;

    /* Struct or union tag name, taken from symbol table in order to be able to
     * print the reference. */
    const char *tag_name;
};

/* 6.2.5 Types. Types are separated into object types and function types. Define
 * macros that mimic semantics given in standardese. */
#define is_object(t) ((t)->type != FUNCTION)
#define is_struct_or_union(t) ((t)->type == OBJECT)
#define is_integer(t) (t->type == INTEGER)
#define is_pointer(t) (t->type == POINTER)
#define is_arithmetic(t) (is_integer(t) || t->type == REAL)
#define is_scalar(t) (is_arithmetic(t) || t->type == POINTER)
#define is_aggregate(t) ((t)->type == ARRAY || is_object(t))
#define is_void(t) ((t)->type == NONE)

#define is_const(t) (t->qualifier & 0x01)
#define is_volatile(t) (t->qualifier & 0x02)
#define is_unsigned(t) (t->flags & 0x01)
#define is_vararg(t) (t->flags & 0x02)
#define is_union(t) (((t)->flags & 0x04) != 0)

struct typetree type_from_specifier(unsigned short spec);

struct typetree *type_init_integer(int size);
struct typetree *type_init_unsigned(int size);
struct typetree *type_init_pointer(const struct typetree *to);
struct typetree *type_init_array(const struct typetree *of, int size);
struct typetree *type_init_function(void);
struct typetree *type_init_object(void);
struct typetree *type_init_void(void);

const struct typetree *type_init_string(size_t length);

int type_equal(const struct typetree *l, const struct typetree *r);

int is_compatible(const struct typetree *l, const struct typetree *r);

/* Instances of OBJECT types can be represented as indirections in the form of
 * a tag. Return the complete object if tagged.
 */
const struct typetree *unwrap_if_indirection(const struct typetree *type);

/* Get the type the given POINTER is pointing to. Handles tag indirections for
 * pointers to typedef'ed object types.
 */
const struct typetree *type_deref(const struct typetree *ptr);

const struct typetree *
type_complete(const struct typetree *, const struct typetree *);

/* Find a common real type between operands used in an expression, giving the
 * type of the result.
 */
const struct typetree *usual_arithmetic_conversion(
    const struct typetree *t1,
    const struct typetree *t2);

/* Promote the given integer type to int or unsigned int, or do nothing if the
 * precision is already as wide. For example, unsigned short will be converted
 * to int.
 */
const struct typetree *promote_integer(const struct typetree *type);

void type_add_member(struct typetree *, const struct typetree *, const char *);

void type_align_struct_members(struct typetree *type);

/* Find type member of the given name, meaning struct or union field, or
 * function parameter. Returns NULL in the case no member is found.
 */
const struct member *find_type_member(
    const struct typetree *type,
    const char *name);

/* Print type to buffer, returning how many characters were written.
 */
int snprinttype(const struct typetree *type, char *str, size_t size);

/* Serialize type to string. Allocates memory with malloc, caller is responsible
 * for calling free.
 */
char *typetostr(const struct typetree *type);

#endif
