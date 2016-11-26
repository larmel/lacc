#include "type.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STAMP_TYPE(t, w) { \
        {t, Q_NONE, w}, \
        {t, Q_CONST, w}, \
        {t, Q_VOLATILE, w}, \
        {t, Q_CONST | Q_VOLATILE, w} \
    }

static const struct typetree
    basic_void_type[4] = STAMP_TYPE(T_VOID, 0),
    basic_signed_type[][4] = {
        STAMP_TYPE(T_SIGNED, 1),
        STAMP_TYPE(T_SIGNED, 2),
        {{0}},
        STAMP_TYPE(T_SIGNED, 4),
        {{0}},
        {{0}},
        {{0}},
        STAMP_TYPE(T_SIGNED, 8)
    },
    basic_unsigned_type[][4] = {
        STAMP_TYPE(T_UNSIGNED, 1),
        STAMP_TYPE(T_UNSIGNED, 2),
        {{0}},
        STAMP_TYPE(T_UNSIGNED, 4),
        {{0}},
        {{0}},
        {{0}},
        STAMP_TYPE(T_UNSIGNED, 8)
    },
    basic_real_type[][4] = {
        STAMP_TYPE(T_REAL, 4),
        STAMP_TYPE(T_REAL, 8)
    };

const struct typetree *get_basic_type(
    enum type type,
    int size,
    enum qualifier cv)
{
    switch (type) {
    default: assert(0);
    case T_SIGNED:
        return &basic_signed_type[size - 1][cv];
    case T_UNSIGNED:
        return &basic_unsigned_type[size - 1][cv];
    case T_REAL:
        return &basic_real_type[(size / 4) - 1][cv];
    case T_VOID:
        return &basic_void_type[cv];
    }
}

const struct typetree
    basic_type__void            = { T_VOID },
    basic_type__const_void      = { T_VOID, Q_CONST, 0 },
    basic_type__char            = { T_SIGNED, Q_NONE, 1 },
    basic_type__short           = { T_SIGNED, Q_NONE, 2 },
    basic_type__int             = { T_SIGNED, Q_NONE, 4 },
    basic_type__long            = { T_SIGNED, Q_NONE, 8 },
    basic_type__unsigned_char   = { T_UNSIGNED, Q_NONE, 1 },
    basic_type__unsigned_short  = { T_UNSIGNED, Q_NONE, 2 },
    basic_type__unsigned_int    = { T_UNSIGNED, Q_NONE, 4 },
    basic_type__unsigned_long   = { T_UNSIGNED, Q_NONE, 8 },
    basic_type__float           = { T_REAL, Q_NONE, 4 },
    basic_type__double          = { T_REAL, Q_NONE, 8 };

/*
 * Store member list separate from type to make memory ownership easier,
 * types do not own their member list.
 */
struct signature {
    unsigned is_vararg : 1;
    array_of(struct member) members;
};

/*
 * Manage memory ownership of all dynamically allocated types and type
 * members, freeing them on exit.
 */
static array_of(struct typetree *) types;
static array_of(struct signature *) signatures;

static void cleanup(void)
{
    int i;
    struct typetree *type;
    struct signature *signature;

    for (i = 0; i < array_len(&types); ++i) {
        type = array_get(&types, i);
        free(type);
    }

    for (i = 0; i < array_len(&signatures); ++i) {
        signature = array_get(&signatures, i);
        array_clear(&signature->members);
        free(signature);
    }

    array_clear(&types);
    array_clear(&signatures);
}

static struct typetree *mktype(void)
{
    static int init;
    struct typetree *type = calloc(1, sizeof(*type));
    array_push_back(&types, type);
    if (!init) {
        init = 1;
        atexit(cleanup);
    }

    return type;
}

static struct signature *get_type_signature(struct typetree *type)
{
    struct signature *sig;

    sig = (struct signature *) type->signature;
    if (!sig) {
        sig = calloc(1, sizeof(*sig));
        array_push_back(&signatures, sig);
        assert(array_len(&types));
        type->signature = sig;
    }

    return sig;
}

size_t type_alignment(const struct typetree *type)
{
    int i;
    size_t m = 0, d;
    assert(is_object(type));

    switch (type->type) {
    case T_ARRAY:
        return type_alignment(type->next);
    case T_STRUCT:
    case T_UNION:
        type = unwrapped(type);
        for (i = 0; i < nmembers(type); ++i) {
            d = type_alignment(get_member(type, i)->type);
            if (d > m) m = d;
        }
        assert(m);
        return m;
    default:
        return type->size;
    }
}

int nmembers(const struct typetree *type)
{
    return (type->signature) ? array_len(&type->signature->members) : 0;
}

const struct member *get_member(const struct typetree *type, int n)
{
    return (!type->signature || array_len(&type->signature->members) <= n)
        ? NULL
        : &array_get(&type->signature->members, n);
}

/*
 * Add member to type signature list. Create the list if this is the
 * first member added. Calculate new size of parent type based on the
 * type added.
 *
 * Verify that a named struct or union member does not already exist.
 */
static void add_member(struct typetree *parent, struct member m)
{
    struct signature *sig;

    sig = get_type_signature(parent);
    if (!str_cmp(m.name, str_init("..."))) {
        assert(!sig->is_vararg);
        assert(is_function(parent));
        sig->is_vararg = 1;
    } else {
        if (m.name.len && find_type_member(parent, m.name)) {
            error("Member '%s' already exists.", str_raw(m.name));
            exit(1);
        }
        array_push_back(&sig->members, m);
        if (is_object(parent)) {
            if (LONG_MAX - m.offset < size_of(m.type)) {
                error("Object is too large.");
                exit(1);
            }
            if (parent->size < m.offset + size_of(m.type)) {
                parent->size = m.offset + size_of(m.type);
            }
        }
    }
}

/*
 * Add necessary padding to parent struct such that new member type can
 * be added. Union types need no padding.
 */
static size_t adjust_member_alignment(
    struct typetree *parent,
    const struct typetree *type)
{
    size_t alignment = 0;

    assert(is_struct_or_union(parent));
    if (is_struct(parent)) {
        alignment = type_alignment(type);
        if (parent->size % alignment) {
            parent->size += alignment - (parent->size % alignment);
            assert(parent->size % alignment == 0);
        }

        alignment = parent->size;
    }

    return alignment;
}

void type_add_member(
    struct typetree *parent,
    String name,
    const struct typetree *type)
{
    struct member m = {0};

    assert(is_struct_or_union(parent) || is_function(parent));
    assert(!is_tagged(parent));
    if (is_function(parent)) {
        if (type && is_array(type)) {
            type = type_init(T_POINTER, type->next);
        }
    } else {
        m.offset = adjust_member_alignment(parent, type);
    }

    m.name = name;
    m.type = type;
    add_member(parent, m);
}

static int pack_field(const struct member *prev, struct member *m)
{
    int bits;

    assert(prev);
    bits = prev->field_offset + prev->field_width;
    if (bits + m->field_width <= size_of(&basic_type__int) * 8) {
        m->offset = prev->offset;
        m->field_offset = bits;
        return 1;
    }

    return 0;
}

const struct member *get_last_field_member(struct signature *sig)
{
    const struct member *prev;
    int count;

    count = array_len(&sig->members);
    if (count) {
        prev = &array_get(&sig->members, count - 1);
        if (prev && prev->field_width) {
            return prev;
        }
    }

    return NULL;
}

void type_add_field(
    struct typetree *parent,
    String name,
    const struct typetree *type,
    int width)
{
    struct member m = {0};
    struct signature *sig;
    const struct member *prev;

    assert(is_struct_or_union(parent));
    assert(!is_tagged(parent));
    assert(type_equal(type, &basic_type__int)
        || type_equal(type, &basic_type__unsigned_int));

    if (name.len && !width) {
        error("Zero length field %s.", str_raw(name));
        exit(1);
    }

    /* Anonymous union fields are ignored, not needed for alignment. */
    if (is_union(parent) && name.len == 0) {
        return;
    }

    m.name = name;
    m.type = type;
    m.field_width = width;
    if (is_struct(parent)) {
        sig = get_type_signature(parent);
        prev = get_last_field_member(sig);
        if (!prev || !pack_field(prev, &m)) {
            m.field_offset = 0;
            m.offset = adjust_member_alignment(parent, type);
        }
    }

    add_member(parent, m);
}

void type_add_anonymous_member(
    struct typetree *parent,
    const struct typetree *type)
{
    int i;
    size_t offset;
    struct member m;

    assert(is_struct_or_union(parent));
    assert(is_struct_or_union(type));
    if (is_struct(parent) && is_union(type)) {
        offset = adjust_member_alignment(parent, type);
        for (i = 0; i < nmembers(type); ++i) {
            m = array_get(&type->signature->members, i);
            m.offset += offset;
            add_member(parent, m);
        }
    } else if (is_union(parent) && is_struct(type)) {
        for (i = 0; i < nmembers(type); ++i) {
            m = array_get(&type->signature->members, i);
            add_member(parent, m);
        }
    } else {
        for (i = 0; i < nmembers(type); ++i) {
            type_add_member(parent, m.name, m.type);
        }
    }
}

/*
 * Adjust aggregate type size to be a multiple of strongest member
 * alignment. This function should only be called once all members have
 * been added.
 */
void type_seal(struct typetree *type)
{
    int i;
    size_t align, maxalign;
    struct signature *sig;
    struct member m;

    maxalign = 0;
    sig = (struct signature *) type->signature;
    for (i = 0; i < nmembers(type); ++i) {
        m = array_get(&sig->members, i);
        if (is_struct(type) && m.name.len == 0) {
            array_erase(&sig->members, i);
            i -= 1;
            continue;
        }
        align = type_alignment(m.type);
        if (align > maxalign) {
            maxalign = align;
        }
    }

    if (maxalign == 0) {
        error("%s has no named members.", is_struct(type) ? "Struct" : "Union");
        exit(1);
    }

    if (type->size % maxalign) {
        type->size += maxalign - (type->size % maxalign);
    }
}

int is_vararg(const struct typetree *type)
{
    assert(is_function(type));
    return (type->signature) ? type->signature->is_vararg : 0;
}

struct typetree *type_init(enum type tt, ...)
{
    struct typetree *type;
    va_list args;
    va_start(args, tt);

    type = mktype();
    type->type = tt;
    if (tt == T_POINTER || tt == T_ARRAY) {
        type->next = va_arg(args, const struct typetree *);
        type->size = 8;
        if (tt == T_ARRAY) {
            type->size = va_arg(args, size_t);
            if (type->size > LONG_MAX / size_of(type->next)) {
                error("Array is too large (%lu elements).", type->size);
                exit(1);
            } else {
                type->size *= size_of(type->next);
            }
        }
    } else if (tt == T_UNSIGNED || tt == T_SIGNED) {
        type->size = va_arg(args, size_t);
        assert(
            type->size == 8 || type->size == 4 ||
            type->size == 2 || type->size == 1);
    } else if (tt == T_FUNCTION) {
        type->next = va_arg(args, const struct typetree *);
    }

    va_end(args);
    return type;
}

const struct typetree *unwrapped(const struct typetree *type)
{
    return is_tagged(type) ? type->next : type;
}

struct typetree *type_tagged_copy(
    const struct typetree *type,
    String name)
{
    struct typetree *tag;

    assert(!is_tagged(type));
    assert(is_struct_or_union(type));

    tag = mktype();
    tag->type = type->type;
    tag->tag = name;
    tag->next = type;
    return tag;
}

/*
 * Determine whether two types are the same. Disregard qualifiers, and
 * names of function parameters.
 */
int type_equal(const struct typetree *a, const struct typetree *b)
{
    int i;
    const struct member *ma, *mb;

    if (!a && !b) return 1;
    if (!a || !b) return 0;
    if (is_tagged(a)) a = unwrapped(a);
    if (is_tagged(b)) b = unwrapped(b);
    if (a == b) return 1;
    if (a->type == b->type
        && a->size == b->size
        && nmembers(a) == nmembers(b)
        && type_equal(a->next, b->next))
    {
        for (i = 0; i < nmembers(a); ++i) {
            ma = get_member(a, i);
            mb = get_member(b, i);
            if (!type_equal(ma->type, mb->type)) {
                return 0;
            } else if (is_struct_or_union(a) && str_cmp(ma->name, mb->name)) {
                return 0;
            } else {
                assert(ma->offset == mb->offset);
            }
        }
    } else {
        return 0;
    }

    return 1;
}

static const struct typetree *remove_qualifiers(const struct typetree *type)
{
    struct typetree *copy;

    if (type->qualifier) {
        copy = mktype();
        assert(!nmembers(type));
        *copy = *type;
        copy->qualifier = Q_NONE;
        type = copy;
    }

    return type;
}

const struct typetree *promote_integer(const struct typetree *type)
{
    assert(is_integer(type));
    if (type->size < 4) {
        type = &basic_type__int;
    }

    return type;
}

const struct typetree *usual_arithmetic_conversion(
    const struct typetree *t1,
    const struct typetree *t2)
{
    const struct typetree *res;

    assert(is_arithmetic(t1) && is_arithmetic(t2));
    if (is_double(t1) || is_double(t2)) {
        res = &basic_type__double;
    } else if (is_float(t1) || is_float(t2)) {
        res = &basic_type__float;
    } else {
        t1 = promote_integer(t1);
        t2 = promote_integer(t2);
        if (t1->size > t2->size) {
            res = t1;
        } else if (t2->size > t1->size) {
            res = t2;
        } else {
            res = is_unsigned(t1) ? t1 : t2;
        }
    }

    return remove_qualifiers(res);
}

/* 6.2.7 Compatible types. Simplified rules. */
int is_compatible(const struct typetree *l, const struct typetree *r)
{
    return type_equal(l, r);
}

size_t size_of(const struct typetree *type)
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
    String name)
{
    int i;
    const struct member *member;

    assert(is_struct_or_union(type) || is_function(type));
    type = unwrapped(type);
    for (i = 0; i < nmembers(type); ++i) {
        member = get_member(type, i);
        if (!str_cmp(name, member->name)) {
            return member;
        }
    }

    return NULL;
}

int fprinttype(FILE *stream, const struct typetree *type)
{
    const struct member *m;
    int i, n = 0;

    if (type == NULL) {
        return fprintf(stream, "(null)");
    }

    if (is_const(type))
        n += fputs("const ", stream);
    if (is_volatile(type))
        n += fputs("volatile ", stream);
    if (is_tagged(type)) {
        assert(is_struct_or_union(type));
        return n + fprintf(stream, "%s %s",
            is_union(type) ? "union" : "struct", str_raw(type->tag));
    }

    switch (type->type) {
    case T_UNSIGNED:
        n += fputs("unsigned ", stream);
    case T_SIGNED:
        n += fputs(
            type->size == 1 ? "char" :
            type->size == 2 ? "short" :
            type->size == 4 ? "int" : "long", stream);
        break;
    case T_REAL:
        n += fputs(is_float(type) ? "float" : "double", stream);
        break;
    case T_VOID:
        n += fputs("void", stream);
        break;
    case T_POINTER:
        n += fputs("* ", stream);
        n += fprinttype(stream, type->next);
        break;
    case T_FUNCTION:
        n += fputs("(", stream);
        for (i = 0; i < nmembers(type); ++i) {
            n += fprinttype(stream, get_member(type, i)->type);
            if (i < nmembers(type) - 1) {
                n += fputs(", ", stream);
            }
        }
        if (is_vararg(type)) {
            n += fputs(", ...", stream);
        }
        n += fputs(") -> ", stream);
        n += fprinttype(stream, type->next);
        break;
    case T_ARRAY:
        if (type->size) {
            n += fprintf(stream, "[%lu] ", type->size / size_of(type->next));
        } else {
            n += fputs("[] ", stream);
        }
        n += fprinttype(stream, type->next);
        break;
    case T_STRUCT:
    case T_UNION:
        n += fputc('{', stream);
        for (i = 0; i < nmembers(type); ++i) {
            m = get_member(type, i);
            n += fprintf(stream, ".%s::", str_raw(m->name));
            n += fprinttype(stream, m->type);
            if (m->field_width) {
                n += fprintf(stream, " (+%lu:%d:%d)",
                    m->offset, m->field_offset, m->field_width);
            } else {
                n += fprintf(stream, " (+%lu)", m->offset);
            }
            if (i < nmembers(type) - 1) {
                n += fputs(", ", stream);
            }
        }
        n += fputc('}', stream);
        break;
    }

    return n;
}
