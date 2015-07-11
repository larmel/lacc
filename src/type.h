#ifndef TYPE_H
#define TYPE_H

#include <stdlib.h>

/* Internal representation of a type. Should fit in four eightbytes.
 */
struct typetree
{
    enum {
        INTEGER,    /* signed or unsigned integer */
        REAL,       /* floating point */
        POINTER,
        FUNCTION,
        ARRAY,
        OBJECT,     /* struct or union */
        NONE        /* void */
    } type;

    int size;       /* Total storage size in bytes, returned for sizeof */

    unsigned short qualifier;   /*         const  | volatile */
    unsigned short flags;       /* union | vararg | unsigned */

    int n;          /* Number of function parameters or object members */

    /* Function parameters or struct/union members. */
    struct member {
        const struct typetree *type;
        const char *name;
        int offset;
    } *member;

    /* Function return value, pointer target, or array base. */
    const struct typetree *next;
};

/* 6.2.5 Types */
#define is_integer(t) (t->type == INTEGER)
#define is_pointer(t) (t->type == POINTER)
#define is_arithmetic(t) (is_integer(t) || t->type == REAL)
#define is_scalar(t) (is_arithmetic(t) || t->type == POINTER)
#define is_aggregate(t) (t->type == ARRAY || t->type == OBJECT)

#define is_const(t) (t->qualifier & 0x01)
#define is_volatile(t) (t->qualifier & 0x02)
#define is_unsigned(t) (t->flags & 0x01)
#define is_vararg(t) (t->flags & 0x02)
#define is_union(t) (t->flags & 0x04)

struct typetree type_from_specifier(unsigned short spec);

struct typetree *type_init_integer(int size);
struct typetree *type_init_unsigned(int size);
struct typetree *type_init_pointer(const struct typetree *to);
struct typetree *type_init_array(const struct typetree *of, int size);
struct typetree *type_init_function(void);
struct typetree *type_init_object(void);
struct typetree *type_init_void(void);

const struct typetree *type_init_string(size_t length);

void type_add_member(struct typetree *, const struct typetree *, const char *);
void type_align_struct_members(struct typetree *type);

int type_equal(const struct typetree *l, const struct typetree *r);
int is_compatible(const struct typetree *l, const struct typetree *r);

const struct typetree *type_deref(const struct typetree *);

const struct typetree *
type_complete(const struct typetree *, const struct typetree *);

const struct typetree *
usual_arithmetic_conversion(const struct typetree *l, const struct typetree *r);

char *typetostr(const struct typetree *type);

#endif
