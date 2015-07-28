#include "error.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct typetree V0 = { NONE };
static struct typetree I1 = { INTEGER, 1 };
static struct typetree I2 = { INTEGER, 2 };
static struct typetree I4 = { INTEGER, 4 };
static struct typetree I8 = { INTEGER, 8 };
static struct typetree U1 = { INTEGER, 1, 0x00, 0x01 };
static struct typetree U2 = { INTEGER, 2, 0x00, 0x01 };
static struct typetree U4 = { INTEGER, 4, 0x00, 0x01 };
static struct typetree U8 = { INTEGER, 8, 0x00, 0x01 };
static struct typetree F4 = { REAL, 4 };
static struct typetree F8 = { REAL, 8 };

struct typetree type_from_specifier(unsigned short spec)
{
    switch (spec) {
    case 0x0001: /* void */
        return V0;
    case 0x0002: /* char */
    case 0x0012: /* signed char */
        return I1;
    case 0x0022: /* unsigned char */
        return U1;
    case 0x0004: /* short */
    case 0x0014: /* signed short */
    case 0x000C: /* short int */
    case 0x001C: /* signed short int */
        return I2;
    case 0x0024: /* unsigned short */
    case 0x002C: /* unsigned short int */
        return U2;
    case 0x0008: /* int */
    case 0x0010: /* signed */
    case 0x0018: /* signed int */
        return I4;
    case 0x0020: /* unsigned */
    case 0x0028: /* unsigned int */
        return U4;
    case 0x0040: /* long */
    case 0x0050: /* signed long */
    case 0x0048: /* long int */
    case 0x0058: /* signed long int */
    case 0x00C0: /* long long */
    case 0x00D0: /* signed long long */
    case 0x00D8: /* signed long long int */
        return I8;
    case 0x0060: /* unsigned long */
    case 0x0068: /* unsigned long int */
    case 0x00E0: /* unsigned long long */
    case 0x00E8: /* unsigned long long int */
        return U8;
    case 0x0100: /* float */
        return F4;
    case 0x0200: /* double */
    case 0x0240: /* long double */
        return F8;
    default:
        error("Invalid type specification.");
        exit(1); 
    }
}

struct typetree *type_init_integer(int width)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = INTEGER;
    type->size = width;
    return type;
}

struct typetree *type_init_unsigned(int width)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = INTEGER;
    type->size = width;
    type->flags = 0x01;
    return type;
}

struct typetree *type_init_pointer(const struct typetree *to)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = POINTER;
    type->size = 8;
    type->next = to;
    return type;
}

struct typetree *type_init_array(const struct typetree *of, int n)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = ARRAY;
    type->size = n * of->size;
    type->next = of;
    return type;
}

struct typetree *type_init_function(void)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = FUNCTION;
    return type;
}

struct typetree *type_init_object(void)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = OBJECT;
    return type;
}

struct typetree *type_init_void(void)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = NONE;
    return type;
}

const struct typetree *type_init_string(size_t length)
{
    return type_init_array(&I1, length);
}

void type_add_member(
    struct typetree *type, const struct typetree *member, const char *name)
{
    type->n++;
    type->member = realloc(type->member, sizeof(*type->member) * type->n);

    type->member[type->n - 1].type = member;
    type->member[type->n - 1].name = name;
    type->member[type->n - 1].offset = 0;
}

void type_align_struct_members(struct typetree *type)
{
    int i = 0, m = 1;

    assert(type->type == OBJECT && type->n);

    for ( ; i < type->n; ++i) {
        struct member *field = &type->member[i];
        int alignment = field->type->size;

        switch (field->type->type) {
        case ARRAY:
            alignment = field->type->next->size;
        case INTEGER:
        case REAL:
        case POINTER:
            if (type->size % alignment) {
                type->size += alignment - (type->size % alignment);
            }
            if (alignment > m) {
                m = alignment;
            }
            break;
        default:
            break;
        }

        field->offset = type->size;
        type->size += field->type->size;
    }

    if (type->size % m) {
        type->size += m - (type->size % m);
    }
}

#define is_tagged(t) ((t)->type == OBJECT && (t)->next)

/* Some object types are represented with a tag, and indirectly pointing to a
 * typedef'ed value. Returns a copy of the typedef, with qualifiers applied.
 */
static const struct typetree *unwrap_if_indirection(const struct typetree *type)
{
    if (type->type == OBJECT && type->next) {
        struct typetree *obj = type_init_object();
        *obj = *type->next;
        obj->qualifier = type->qualifier;
        type = obj;
    }
    return type;
}

/* Determine whether two types are the same.
 */
int type_equal(const struct typetree *a, const struct typetree *b)
{
    if (!a && !b) return 1;
    if (!a || !b) return 0;
    if (is_tagged(a) && is_tagged(b))
        return a->next == b->next && a->qualifier == b->qualifier;

    a = unwrap_if_indirection(a);
    b = unwrap_if_indirection(b);

    if (a->type == b->type
        && a->size == b->size
        && a->n == b->n
        /*&& a->qualifier == b->qualifier*/
        && is_unsigned(a) == is_unsigned(b)
        && type_equal(a->next, b->next))
    {
        int i;
        for (i = 0; i < a->n; ++i) {
            if (!type_equal(a->member[i].type, b->member[i].type)) {
                return 0;
            }
        }
        return 1;   
    }

    return 0;
}

/* 6.3.1.8 Usual Arithmetic Conversion. Find a common real type between the
 * operands. Each operand is converted to common type, and unless otherwise
 * specified this is also the result type.
 */
const struct typetree *
usual_arithmetic_conversion(const struct typetree *l, const struct typetree *r)
{
    assert( is_arithmetic(l) && is_arithmetic(r) );

    /* Skip everything dealing with floating point types. */

    if (type_equal(l, r)) return l;
    if (is_unsigned(l) == is_unsigned(r)) return (l->size > r->size) ? l : r;

    /* Make sure l is signed and r is unsigned */
    if (is_unsigned(l) && !is_unsigned(r)) {
        return usual_arithmetic_conversion(r, l);
    }

    assert( !is_unsigned(l) && is_unsigned(r) );

    /* Integer promotion. This could be separated out, as it is not only 
     * performed for usual arithmetic conversion. This may also need to be in 
     * eval.c, as it can generate temporaries (i.e. if a char is promoted to
     * int). */
    if (l->size > r->size) return l;
    if (l->size == r->size) return r;

    return r;
}

/* 6.2.7 Compatible types. Simplified rules.
 */
int is_compatible(const struct typetree *l, const struct typetree *r)
{
    assert( is_pointer(l) && is_pointer(r) );

    return type_equal(l, r) || (l->next->size == r->next->size);
}

const struct typetree *type_deref(const struct typetree *ptr)
{
    assert(ptr->type == POINTER);
    return unwrap_if_indirection(ptr->next);
}

const struct typetree *get_return_type(const struct typetree *func)
{
    assert(func->type == FUNCTION);
    return unwrap_if_indirection(func->next);
}

/* Validate that type p can be completed by applying size from q, and return q
 * as the result.
 */
const struct typetree *
type_complete(const struct typetree *p, const struct typetree *q)
{
    /* Functions have no size, quick fix to avoid rejecting functions that are
     * declared more than once. */
    assert(p->type == FUNCTION || (!p->size && q->size));

    if (p->type != q->type || !type_equal(p->next, q->next)) {
        error("Incompatible specification of incomplete type.");
        exit(1);
    }

    return q;
}

const struct member *find_type_member(
    const struct typetree *type,
    const char *name)
{
    int i;

    if (type->type != OBJECT) {
        error("Cannot access field of non-object type.");
    } else {
        for (i = 0; i < type->n; ++i) {
            if (!strcmp(name, type->member[i].name)) {
                return type->member + i;
            }
        }
    }
    return NULL;
}

/* Print type to buffer, returning how many characters were written.
 */
static int snprinttype(const struct typetree *tree, char *s, int size)
{
    int i, w = 0;

    if (!tree) {
        return w;
    }

    if (is_const(tree))     w += snprintf(s + w, size - w, "const ");
    if (is_volatile(tree))  w += snprintf(s + w, size - w, "volatile ");

    if (is_tagged(tree)) {
        w += snprintf(s + w, size - w, "%s %s",
            (tree->flags & 0x04) ? "union" : "struct", tree->tag_name);
        assert( tree->type == OBJECT );
        return w;
    }

    if (is_unsigned(tree))  w += snprintf(s + w, size - w, "unsigned ");

    switch (tree->type) {
    case INTEGER:
        switch (tree->size) {
        case 1:
            w += snprintf(s + w, size - w, "char");
            break;
        case 2:
            w += snprintf(s + w, size - w, "short");
            break;
        case 4:
            w += snprintf(s + w, size - w, "int");
            break;
        default:
            w += snprintf(s + w, size - w, "long");
            break;
        }
        break;
    case REAL:
        switch (tree->size) {
        case 4:
            w += snprintf(s + w, size - w, "float");
            break;
        default:
            w += snprintf(s + w, size - w, "double");
            break;
        }
        break;
    case NONE:
        w += snprintf(s + w, size - w, "void");
        break;
    case POINTER:
        w += snprintf(s + w, size - w, "* ");
        w += snprinttype(tree->next, s + w, size - w);
        break;
    case FUNCTION:
        w += snprintf(s + w, size - w, "(");
        for (i = 0; i < tree->n; ++i) {
            w += snprinttype(tree->member[i].type, s + w, size - w);
            if (i < tree->n - 1) {
                w += snprintf(s + w, size - w, ", ");
            }
        }
        w += snprintf(s + w, size - w, ") -> ");
        w += snprinttype(tree->next, s + w, size - w);
        break;
    case ARRAY:
        if (tree->size > 0) {
            w += snprintf(s + w, size - w, "[%u] ",
                tree->size / tree->next->size);
        } else {
            w += snprintf(s + w, size - w, "[] ");
        }
        w += snprinttype(tree->next, s + w, size - w);
        break;
    case OBJECT:
        w += snprintf(s + w, size - w, "{");
        for (i = 0; i < tree->n; ++i) {
            w += snprintf(s + w, size - w, ".%s::", tree->member[i].name);
            w += snprinttype(tree->member[i].type, s + w, size - w);
            w += snprintf(s + w, size - w, " (+%d)", tree->member[i].offset);
            if (i < tree->n - 1) {
                w += snprintf(s + w, size - w, ", ");
            }
        }
        w += snprintf(s + w, size - w, "}");
        break;
    default:
        break;
    }

    return w;
}

/* For debug printing and error reporting types. Caller should free memory. */
char *typetostr(const struct typetree *type)
{
    char *text = malloc(2048 * sizeof(char));
    snprinttype(type, text, 2047);
    return text;
}
