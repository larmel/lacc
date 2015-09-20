#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
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

static struct typetree **type_registry;
static size_t length;
static size_t cap;

static void cleanup(void)
{
    size_t i = 0;
    struct typetree *t;

    for ( ; i < length; ++i) {
        t = type_registry[i];
        free(t);
    }
}

static struct typetree *alloctype(struct typetree args)
{
    if (!length)
        atexit(cleanup);

    if (length == cap) {
        cap = (!cap) ? 32 : cap * 2;
        type_registry = realloc(type_registry, cap * sizeof(*type_registry));
    }

    type_registry[length] = calloc(1, sizeof(**type_registry));
    *type_registry[length] = args;
    return type_registry[length++];
}

void type_add_member(
    struct typetree *type,
    const struct typetree *member,
    const char *name)
{
    type->n++;
    type->member = realloc(type->member, sizeof(*type->member) * type->n);

    type->member[type->n - 1].type = member;
    type->member[type->n - 1].name = name;
    type->member[type->n - 1].offset = 0;
}

struct typetree *type_init_integer(int width)
{
    struct typetree arg = { INTEGER };
    arg.size = width;
    assert(width == 1 || width == 2 || width == 4 || width == 8);

    return alloctype(arg);
}

struct typetree *type_init_unsigned(int width)
{
    struct typetree arg = { INTEGER, 0, 0x01 };
    arg.size = width;
    assert(width == 1 || width == 2 || width == 4 || width == 8);

    return alloctype(arg);
}

struct typetree *type_init_pointer(const struct typetree *to)
{
    struct typetree arg = { POINTER, 8 };
    arg.next = to;

    return alloctype(arg);
}

struct typetree *type_init_array(const struct typetree *child, int n)
{
    struct typetree arg = { ARRAY };
    arg.size = n * child->size;
    arg.next = child;

    return alloctype(arg);
}

struct typetree *type_init_function(void)
{
    struct typetree arg = { FUNCTION };

    return alloctype(arg);
}

struct typetree *type_init_object(void)
{
    struct typetree arg = { OBJECT };

    return alloctype(arg);
}

struct typetree *type_init_void(void)
{
    struct typetree arg = { NONE };

    return alloctype(arg);
}

const struct typetree *type_init_string(size_t length)
{
    return type_init_array(&I1, length);
}

void type_align_struct_members(struct typetree *type)
{
    int i = 0, m = 1;

    assert(!is_tagged(type));
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

const struct typetree *unwrapped(const struct typetree *type)
{
    return is_tagged(type) ? type->next : type;
}

struct typetree *type_tagged_copy(const struct typetree *type, const char *name)
{
    struct typetree *tag;

    assert(!is_tagged(type));
    assert(type->type == OBJECT);

    tag = type_init_object();
    tag->tag_name = name;
    tag->next = type;
    return tag;
}

/* Determine whether two types are the same. Disregarding qualifiers, and names
 * of function parameters.
 */
int type_equal(const struct typetree *a, const struct typetree *b)
{
    if (!a && !b) return 1;
    if (!a || !b) return 0;
    if (is_tagged(a) && is_tagged(b))
        return a->next == b->next;

    a = unwrapped(a);
    b = unwrapped(b);

    if (a->type == b->type
        && a->size == b->size
        && a->n == b->n
        && is_unsigned(a) == is_unsigned(b)
        && type_equal(a->next, b->next))
    {
        int i;
        for (i = 0; i < a->n; ++i) {
            if (!type_equal(a->member[i].type, b->member[i].type)) {
                return 0;
            }
            if (is_struct_or_union(a)
                && strcmp(a->member[i].name, b->member[i].name))
            {
                return 0;
            }
            assert(a->member[i].offset == b->member[i].offset);
        }
        return 1;   
    }

    return 0;
}

static const struct typetree *remove_qualifiers(const struct typetree *type)
{
    if (type->qualifier) {
        struct typetree *copy = alloctype(*type);
        assert(!type->n);
        copy->qualifier = 0;
        type = copy;
    }

    return type;
}

const struct typetree *promote_integer(const struct typetree *type)
{
    assert(is_integer(type));
    if (type->size < 4) {
        type = (is_unsigned(type)) ? &U4 : &I4;
    }
    return type;
}

const struct typetree *usual_arithmetic_conversion(
    const struct typetree *t1,
    const struct typetree *t2)
{
    assert(is_arithmetic(t1) && is_arithmetic(t2));

    /* Skip everything dealing with floating point types. */

    assert(is_integer(t1) && is_integer(t2));
    t1 = promote_integer(t1);
    t2 = promote_integer(t2);

    if (t1->size > t2->size)
        /* TODO: This can be done without extra copies. */
        return remove_qualifiers(t1);
    else if (t2->size > t1->size)
        return remove_qualifiers(t2);

    return is_unsigned(t1) ? remove_qualifiers(t1) : remove_qualifiers(t2);
}

/* 6.2.7 Compatible types. Simplified rules.
 */
int is_compatible(const struct typetree *l, const struct typetree *r)
{
    return type_equal(l, r);
}

int size_of(const struct typetree *type)
{
    return is_tagged(type) ? type->next->size : type->size;
}

const struct typetree *type_deref(const struct typetree *type)
{
    assert(is_pointer(type));
    return unwrapped(type->next);
}

const struct member *find_type_member(
    const struct typetree *type,
    const char *name)
{
    int i = 0;
    assert(is_struct_or_union(type));

    type = unwrapped(type);
    for (; i < type->n; ++i) {
        if (!strcmp(name, type->member[i].name)) {
            return type->member + i;
        }
    }
    return NULL;
}

int snprinttype(const struct typetree *tree, char *s, size_t size)
{
    size_t w = 0;
    int i;

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
