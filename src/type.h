#ifndef TYPE_H
#define TYPE_H

#include <stdlib.h>

/* Internal representation of a type.
 */
struct typetree
{
    enum {
        INTEGER,    /* (unsigned) char, short, int, long */ 
        REAL,       /* float, double */
        POINTER,
        FUNCTION,
        ARRAY,
        OBJECT,     /* struct, union */
        NONE        /* void */
    } type;

    int is_const;
    int is_volatile;
    int is_unsigned;
    int is_vararg;  /* Function takes variable argument list, ... */

    int size;       /* Total storage size in bytes, returned for sizeof( ). */
    int n;          /* Number of function parameters or object members. */

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

struct typetree *type_init_integer(int);
struct typetree *type_init_pointer(const struct typetree *);
struct typetree *type_init_array(const struct typetree *, int);
struct typetree *type_init_function(void);
struct typetree *type_init_object(void);
struct typetree *type_init_void(void);

const struct typetree *type_init_string(size_t);

void type_add_member(struct typetree *, const struct typetree *, const char *);
void type_align_struct_members(struct typetree *);

int type_equal(const struct typetree *, const struct typetree *);

int is_compatible(const struct typetree *, const struct typetree *);

const struct typetree *type_deref(const struct typetree *);
const struct typetree *type_complete(const struct typetree *,
                                     const struct typetree *);

const struct typetree *usual_arithmetic_conversion(const struct typetree *l,
                                                   const struct typetree *r);

char *typetostr(const struct typetree *);

#endif
