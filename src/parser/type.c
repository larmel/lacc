#include "type.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const struct typetree
    basic_type__void = { T_VOID },
    basic_type__const_void = { T_VOID, 0, Q_CONST },
    basic_type__char = { T_SIGNED, 1 },
    basic_type__short = { T_SIGNED, 2 },
    basic_type__int = { T_SIGNED, 4 },
    basic_type__long = { T_SIGNED, 8 },
    basic_type__unsigned_char = { T_UNSIGNED, 1 },
    basic_type__unsigned_short = { T_UNSIGNED, 2 },
    basic_type__unsigned_int = { T_UNSIGNED, 4 },
    basic_type__unsigned_long = { T_UNSIGNED, 8 },
    basic_type__float = { T_REAL, 4 },
    basic_type__double = { T_REAL, 8 };

/*
 * Store member list separate from type to make memory ownership easier,
 * types do not own their member list.
 */
struct signature {
    int is_vararg;
    array_of(struct member) members;
};

/*
 * Manage memory ownership of all dynamically allocated types and type
 * members, freeing them on exit.
 */
static array_of(struct typetree *) types;
static array_of(struct signature *) signatures;
static int init;

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
    struct typetree *type = calloc(1, sizeof(*type));
    array_push_back(&types, type);
    if (!init) {
        init = 1;
        atexit(cleanup);
    }
    return type;
}

static struct signature *mksignature(void)
{
    struct signature *sig = calloc(1, sizeof(*sig));
    array_push_back(&signatures, sig);
    if (!init) {
        init = 1;
        atexit(cleanup);
    }
    return sig;
}

int type_alignment(const struct typetree *type)
{
    int i = 0, m = 0, d;
    assert(is_object(type));

    switch (type->type) {
    case T_ARRAY:
        return type_alignment(type->next);
    case T_STRUCT:
    case T_UNION:
        type = unwrapped(type);
        for (; i < nmembers(type); ++i) {
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
    return
        (!type->signature || array_len(&type->signature->members) <= n) ?
            NULL :
            &array_get(&type->signature->members, n);
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

    sig = (struct signature *) parent->signature;
    if (!sig) {
        sig = mksignature();
        parent->signature = sig;
    }

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
        if (is_object(parent) && parent->size < m.offset + size_of(m.type)) {
            parent->size = m.offset + size_of(m.type);
        }
    }
}

/*
 * Add necessary padding to parent struct such that new member type can
 * be added. Union types need no padding.
 */
static int adjust_member_alignment(
    struct typetree *parent,
    const struct typetree *type)
{
    int alignment = 0;

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
    struct member m = {{{0}}};

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

void type_add_field(
    struct typetree *parent,
    String name,
    const struct typetree *type,
    int width)
{
    struct member m = {{{0}}};

    assert(is_struct_or_union(parent));
    assert(!is_tagged(parent));
    assert(type_equal(type, &basic_type__int)
        || type_equal(type, &basic_type__unsigned_int));

    if (name.len && width) {
        m.name = name;
        m.type = type;
        m.width = width;
        m.offset = adjust_member_alignment(parent, type);
        add_member(parent, m);
    }
}

void type_add_anonymous_member(
    struct typetree *parent,
    const struct typetree *type)
{
    int i, offset;
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
void type_seal(struct typetree *parent)
{
    int i, align, maxalign = 0;
    struct member m;

    for (i = 0; i < nmembers(parent); ++i) {
        m = array_get(&parent->signature->members, i);
        align = type_alignment(m.type);
        if (align > maxalign) {
            maxalign = align;
        }
    }

    if (parent->size % maxalign) {
        parent->size += maxalign - (parent->size % maxalign);
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
            type->size = size_of(type->next) * va_arg(args, int);
        }
    } else if (tt == T_UNSIGNED || tt == T_SIGNED) {
        type->size = va_arg(args, int);
        assert(
            type->size == 8 || type->size == 4 ||
            type->size == 2 || type->size == 1);
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
 * Determine whether two types are the same. Disregarding qualifiers,
 * and names of function parameters.
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
        && nmembers(a) == nmembers(b)
        && type_equal(a->next, b->next))
    {
        int i;
        for (i = 0; i < nmembers(a); ++i) {
            if (!type_equal(get_member(a, i)->type, get_member(b, i)->type)) {
                return 0;
            }
            if (is_struct_or_union(a)
                && str_cmp(get_member(a, i)->name, get_member(b, i)->name))
            {
                return 0;
            }
            assert(get_member(a, i)->offset == get_member(b, i)->offset);
        }
        return 1;   
    }

    return 0;
}

static const struct typetree *remove_qualifiers(const struct typetree *type)
{
    if (type->qualifier) {
        struct typetree *copy = mktype();
        assert(!nmembers(type));

        *copy = *type;
        copy->qualifier = 0;
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
        if (type->size > 0) {
            n += fprintf(stream, "[%u]", type->size / size_of(type->next));
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
            n += fprintf(stream, " (+%d)", m->offset);
            if (i < nmembers(type) - 1) {
                n += fputs(", ", stream);
            }
        }
        n += fputc('}', stream);
        break;
    }

    return n;
}
