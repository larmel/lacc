#include "error.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct typetree *type_init_integer(int width)
{
    struct typetree *type = calloc(1, sizeof(*type));
    type->type = INTEGER;
    type->size = width;
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

const typetree_t *type_init_string(size_t length)
{
    static typetree_t *base;

    if (!base) {
        base = type_init_integer(1);
    }

    return type_init_array(base, length);
}

void type_add_member(
    struct typetree *type, const struct typetree *member, const char *name)
{
    type->n++;
    type->member = realloc(type->member, sizeof(*type->member) * type->n);

    type->member[type->n - 1].type = member;
    type->member[type->n - 1].name = name;
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

int type_equal(const typetree_t *a, const typetree_t *b)
{
    if (!a && !b) return 1;
    if (!a || !b) return 0;

    if (a->type == b->type
        && a->size == b->size
        && a->n == b->n
        && a->is_unsigned == b->is_unsigned
        && type_equal(a->next, b->next))
    {
        int i;

        for (i = 0; i < a->n; ++i) {
            if (!type_equal(a->member[i].type, b->member[i].type))
                return 0;
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
    assert(is_arithmetic(l) && is_arithmetic(r));

    /* Skip everything dealing with floating point types. */

    if (type_equal(l, r)) return l;
    if (l->is_unsigned == r->is_unsigned) return (l->size > r->size) ? l : r;

    /* Make sure l is signed and r is unsigned */
    if (l->is_unsigned && !r->is_unsigned) {
        return usual_arithmetic_conversion(r, l);
    }

    assert(!l->is_unsigned && r->is_unsigned);

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

    return (type_equal(l, r) || (l->next->size == r->next->size));
}

const typetree_t *type_deref(const typetree_t *t)
{
    if (t->type != POINTER) {
        char *str = typetostr(t);
        error("Cannot dereference non-pointer type `%s`.", str);
        free(str);
        return NULL;
    }
    return t->next;
}

/* Validate that type p can be completed by applying size from q, and return
 * q as the result.
 */
const typetree_t *type_complete(const typetree_t *p, const typetree_t *q)
{
    assert(!p->size && q->size);

    if (p->type != q->type || !type_equal(p->next, q->next)) {
        error("Incompatible specification of incomplete type.");
        exit(1);
    }

    return q;
}

/* Print type to buffer, returning how many characters were written.
 */
static int snprinttype(const typetree_t *tree, char *s, int size)
{
    int i, w = 0;
    if (!tree)
        return w;

    if (tree->is_unsigned)
        w += snprintf(s + w, size - w, "unsigned ");
    if (tree->is_const)
        w += snprintf(s + w, size - w, "const ");
    if (tree->is_volatile)
        w += snprintf(s + w, size - w, "volatile ");

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
        case 8:
            w += snprintf(s + w, size - w, "long");
            break;
        default:
            w += snprintf(s + w, size - w, "__int%d", tree->size * 8);
            break;
        }
        break;
    case REAL:
        switch (tree->size) {
        case 4:
            w += snprintf(s + w, size - w, "float");
            break;
        case 8:
            w += snprintf(s + w, size - w, "double");
            break;
        default:
            w += snprintf(s + w, size - w, "__real%d", tree->size * 8);
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
char *typetostr(const typetree_t *type)
{
    char *text = malloc(512 * sizeof(char));
    snprinttype(type, text, 511);
    return text;
}
