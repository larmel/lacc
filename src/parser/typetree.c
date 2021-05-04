#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "typetree.h"
#include <lacc/array.h>
#include <lacc/context.h>
#include <lacc/symbol.h>

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

INTERNAL const Type
    basic_type__void           = { T_VOID },
    basic_type__bool           = { T_BOOL },
    basic_type__char           = { T_CHAR },
    basic_type__short          = { T_SHORT },
    basic_type__int            = { T_INT },
    basic_type__long           = { T_LONG },
    basic_type__unsigned_char  = { T_CHAR, 1 },
    basic_type__unsigned_short = { T_SHORT, 1 },
    basic_type__unsigned_int   = { T_INT, 1 },
    basic_type__unsigned_long  = { T_LONG, 1 },
    basic_type__float          = { T_FLOAT },
    basic_type__double         = { T_DOUBLE },
    basic_type__long_double    = { T_LDOUBLE };

/*
 * Hidden full representation of types. Type objects reference one of
 * these for aggregate and nested structures.
 */
struct typetree {
    enum type type;
    unsigned int is_unsigned : 1;
    unsigned int is_const : 1;
    unsigned int is_volatile : 1;
    unsigned int is_restrict : 1;
    unsigned int is_vararg : 1;
    unsigned int is_flexible : 1;
    unsigned int is_vla : 1;
    unsigned int is_incomplete : 1;

    /*
     * Total storage size in bytes for struct, union and basic types,
     * equal to what is returned for sizeof. Number of elements in case
     * of array type.
     */
    size_t size;

    /*
     * Symbol holding size of variable length array. Type of variable is
     * always size_t (unsigned long).
     *
     * Special value NULL means any length '*', if is_vla is set. The
     * flag is always set if vlen is not NULL.
     *
     * Normal size member is zero for VLA types, and size_of still
     * returns 0 without error.
     */
    const struct symbol *vlen;

    /* Function parameters, or struct/union members. */
    array_of(struct member) members;

    /*
     * Function return value, pointer target, array base, or pointer to
     * tagged struct or union type. Tag indirections are used to avoid
     * loops in type trees.
     */
    Type next;

    /*
     * Reference to typedef, or struct, union or enum tag. Stored in
     * order to be able to print self-referential types.
     */
    const struct symbol *tag;
};

/*
 * All types have a number, indexing into a global list. This list only
 * grows.
 */
static array_of(struct typetree) types;

static struct typetree *get_typetree_handle(int ref)
{
    assert(ref > 0);
    assert(ref <= array_len(&types));

    return &array_get(&types, ref - 1);
}

static Type get_type_handle(int ref)
{
    Type type = {0};
    struct typetree *t;

    t = get_typetree_handle(ref);
    switch (t->type) {
    case T_POINTER:
        if (t->next.is_pointer) {
            assert(0);
        } else {
            type.type = t->next.type;
            type.is_unsigned = t->next.is_unsigned;
            type.is_const = t->next.is_const;
            type.is_volatile = t->next.is_volatile;
            type.is_restrict = t->next.is_restrict;
            type.ref = t->next.ref;
            type.is_pointer = 1;
            type.is_pointer_const = t->is_const;
            type.is_pointer_volatile = t->is_volatile;
            type.is_pointer_restrict = t->is_restrict;
        }
        break;
    case T_FUNCTION:
    case T_ARRAY:
    case T_STRUCT:
    case T_UNION:
        type.ref = ref;
    default:
        type.type = t->type;
        type.is_unsigned = t->is_unsigned;
        type.is_volatile = t->is_volatile;
        type.is_const = t->is_const;
        type.is_restrict = t->is_restrict;
        break;
    }

    return type;
}

INTERNAL Type type_create(enum type tt)
{
    Type type = {0};
    struct typetree t = {0};

    t.type = tt;
    array_push_back(&types, t);
    type.type = tt;
    type.ref = array_len(&types);
    return type;
}

INTERNAL void clear_types(FILE *stream)
{
    int i;
    Type type;
    struct typetree *t;

    if (stream) {
        for (i = 0; i < array_len(&types); ++i) {
            type = get_type_handle(i + 1);
            fprinttype(stream, type, NULL);
            putc('\n', stream);
        }
    }

    for (i = 0; i < array_len(&types); ++i) {
        t = &array_get(&types, i);
        array_clear(&t->members);
    }

    array_clear(&types);
}

INTERNAL int is_type_placeholder(Type type)
{
    return type.type == -1;
}

INTERNAL Type get_type_placeholder(void)
{
    Type t = {-1};
    return t;
}

INTERNAL Type type_unqualified(Type type)
{
    if (type.is_pointer) {
        type.is_pointer_const = 0;
        type.is_pointer_volatile = 0;
        type.is_pointer_restrict = 0;
    } else {
        type.is_const = 0;
        type.is_volatile = 0;
        type.is_restrict = 0;
    }

    return type;
}

/*
 * Add member to type signature list. Create the list if this is the
 * first member added. Calculate new size of parent type based on the
 * type added.
 *
 * Verify that a named struct or union member does not already exist.
 */
static struct member *add_member(Type parent, struct member m)
{
    static String dots = SHORT_STRING_INIT("...");

    struct typetree *t;

    assert(is_struct_or_union(parent) || is_function(parent));
    t = get_typetree_handle(parent.ref);
    if (str_eq(m.name, dots)) {
        assert(!t->is_vararg);
        assert(is_function(parent));
        t->is_vararg = 1;
        assert(!t->is_incomplete);
        return NULL;
    }

    if (!str_is_empty(m.name) && find_type_member(parent, m.name, NULL)) {
        error("Member '%s' already exists.", str_raw(m.name));
        exit(1);
    }

    array_push_back(&t->members, m);
    if (is_struct_or_union(parent)) {
        if (size_of(m.type) == 0) {
            if (is_array(m.type) && !t->is_flexible) {
                t->is_flexible = 1;
            } else {
                error("Member '%s' has incomplete type.", str_raw(m.name));
                exit(1);
            }
        }
        if (is_flexible(m.type)) {
            if (is_struct(parent)) {
                error("Cannot add flexible struct member.");
                exit(1);
            }
            t->is_flexible = 1;
        }
        if (LONG_MAX - m.offset < size_of(m.type)) {
            error("Object is too large.");
            exit(1);
        }
        if (t->size < m.offset + size_of(m.type)) {
            t->size = m.offset + size_of(m.type);
        }
    } else {
        t->is_incomplete = is_type_placeholder(m.type);
    }

    return &array_back(&t->members);
}

static Type backing_type_from_bits(int backing_bits)
{
    switch (backing_bits) {
    default: assert(0);
    case 8:
        return basic_type__char;
    case 16:
        return basic_type__short;
    case 32:
        return basic_type__int;
    case 64:
        return basic_type__long;
    }
}

/*
 * Adjust alignment to next integer width after adding an unnamed zero-
 * width field member.
 *
 * Insert a new field member with the appropriate padding bits. If the
 * previous member is not a field, insert padding to align the next
 * member to integer width.
 *
 * If there are no members, nothing to do. Starting with a zero width
 * field does not put the next member at offset other than 0.
 */
static void reset_field_alignment(Type type, Type clear)
{
    struct typetree *t;
    int bits, clear_bits, mod;
    size_t len, clear_size;
    Type backing;
    String name = SHORT_STRING_INIT("");
    const struct member *m;

    assert(is_struct(type));
    t = get_typetree_handle(type.ref);
    len = array_len(&t->members);
    if (!len) {
        return;
    }

    clear_size = size_of(clear);
    clear_bits = clear_size << 3;
    m = &array_get(&t->members, len - 1);
    if (m->field_width) {
        bits = m->field_offset + m->field_width;
        backing = backing_type_from_bits(m->field_backing);
        if (clear_bits >= m->field_backing) {
            assert(bits <= m->field_backing);
            if (bits != m->field_backing) {
                type_add_field(type, name, backing, m->field_backing - bits);
            }
        } else if (bits > clear_bits) {
            mod = bits % clear_bits;
            if (mod) {
                type_add_field(type, name, backing, mod);
            }
        } else {
            type_add_field(type, name, backing, clear_bits - bits);
        }
    }

    if (t->size < clear_size) {
        t->size = clear_size;
    } else if (t->size % clear_size != 0) {
        t->size += t->size % clear_size;
    }
}

/*
 * Add necessary padding to parent struct such that new member type can
 * be added. Union types need no padding.
 *
 * In case a member is added after a bit field, allow overlapping the
 * field backing type if there is room for it.
 */
static size_t adjust_member_alignment(Type parent, Type type)
{
    struct typetree *t;
    struct member *mb;
    size_t align, bytes, offset;
    int i;

    assert(is_struct_or_union(parent));
    if (!is_struct(parent))
        return 0;

    t = get_typetree_handle(parent.ref);
    i = array_len(&t->members) - 1;
    if (i >= 0) {
        mb = &array_get(&t->members, i);
        if (mb->field_backing) {
            offset = mb->offset;
            bytes = mb->field_backing - (mb->field_offset + mb->field_width);
            bytes = bytes / 8;
            assert(bytes >= 0);
            if (bytes) {
                t->size -= bytes;
                while (mb->offset == offset) {
                    mb->field_backing -= bytes * 8;
                    i--;
                    if (i < 0)
                        break;
                    mb = &array_get(&t->members, i);
                }
            }
        }
    }

    align = type_alignment(type);
    if (t->size % align) {
        t->size += align - (t->size % align);
        assert(t->size % align == 0);
    }

    return t->size;
}

INTERNAL Type type_create_pointer(Type next)
{
    Type type;
    struct typetree *t;

    if (next.is_pointer) {
        type = type_create(T_POINTER);
        t = get_typetree_handle(type.ref);
        t->is_const = is_const(next);
        t->is_volatile = is_volatile(next);
        next = type_unqualified(next);
        next.is_pointer = 0;
        t->next = next;
    } else {
        type = next;
        type.is_pointer = 1;
    }

    return type;
}

INTERNAL Type type_create_function(Type next)
{
    Type type;
    struct typetree *t;

    type = type_create(T_FUNCTION);
    t = get_typetree_handle(type.ref);
    t->next = next;
    t->is_incomplete = 1;
    return type;
}

INTERNAL Type type_create_array(Type next, size_t count)
{
    Type type;
    struct typetree *t;

    if (count * size_of(next) > LONG_MAX) {
        error("Array is too large (%lu elements).", count);
        exit(1);
    }

    type = type_create(T_ARRAY);
    t = get_typetree_handle(type.ref);
    t->size = count;
    t->next = next;
    return type;
}

INTERNAL Type type_create_incomplete(Type next)
{
    Type type;
    struct typetree *t;

    type = type_create(T_ARRAY);
    t = get_typetree_handle(type.ref);
    t->next = next;
    t->is_incomplete = 1;
    return type;
}

INTERNAL Type type_create_vla(Type next, const struct symbol *count)
{
    Type type;
    struct typetree *t;

    assert(count);
    type = type_create_array(next, 0);
    t = get_typetree_handle(type.ref);
    t->vlen = count;
    t->is_vla = 1;
    return type;
}

INTERNAL Type type_set_const(Type type)
{
    if (type.is_pointer) {
        type.is_pointer_const = 1;
    } else {
        type.is_const = 1;
    }

    return type;
}

INTERNAL Type type_set_volatile(Type type)
{
    if (type.is_pointer) {
        type.is_pointer_volatile = 1;
    } else {
        type.is_volatile = 1;
    }

    return type;
}

INTERNAL Type type_set_restrict(Type type)
{
    if (!is_pointer(type)) {
        error("Cannot apply 'restrict' qualifier to non-pointer types.");
        exit(1);
    }

    if (type.is_pointer) {
        type.is_pointer_restrict = 1;
    } else {
        type.is_restrict = 1;
    }

    return type;
}

INTERNAL Type type_apply_qualifiers(Type type, Type other)
{
    if (is_const(other))
        type = type_set_const(type);
    if (is_volatile(other))
        type = type_set_volatile(type);
    if (is_restrict(other))
        type = type_set_restrict(type);
    return type;
}

INTERNAL Type type_patch_declarator(Type head, Type target)
{
    struct typetree *t;
    Type next;

    if (is_void(head)) {
        next = target;
    } else {
        if (is_pointer(head)) {
            next = type_next(head);
            next = type_patch_declarator(next, target);
            next = type_create_pointer(next);
            next = type_apply_qualifiers(next, head);
        } else {
            assert(is_function(head) || is_array(head));
            t = get_typetree_handle(head.ref);
            t->next = type_patch_declarator(t->next, target);
            next = head;
        }
    }

    return next;
}

INTERNAL void type_clean_prototype(Type type)
{
    int i;
    struct member *m;
    struct typetree *t;

    switch (type_of(type)) {
    case T_POINTER:
        type_clean_prototype(type_next(type));
        break;
    case T_ARRAY:
        t = get_typetree_handle(type.ref);
        if (t->is_vla) {
            t->vlen = NULL;
        }
        type_clean_prototype(t->next);
        break;
    case T_STRUCT:
    case T_UNION:
        t = get_typetree_handle(type.ref);
        if (t->tag)
            break;
    case T_FUNCTION:
        for (i = 0; i < nmembers(type); ++i) {
            m = get_member(type, i);
            m->sym = NULL;
            type_clean_prototype(m->type);
        }
        break;
    }
}

/*
 * Associate type with a tag symbol, which can be used to print self-
 * referential struct or union types, or typedef'ed objects.
 *
 * This is only relevant for diagnostics, and since basic types do not
 * have room to store a tag, just ignore that case. This means that if
 * you have following, the verbose output of a will be int, not i32.
 *
 *  typedef int i32;
 *  i32 a;
 * 
 */
INTERNAL void type_set_tag(Type type, const struct symbol *tag)
{
    struct typetree *t;

    assert(tag->symtype == SYM_TAG || tag->symtype == SYM_TYPEDEF);
    if (type.ref) {
        t = get_typetree_handle(type.ref);
        if (!t->tag || tag->symtype != SYM_TYPEDEF) {
            t->tag = tag;
        }
    }
}

INTERNAL size_t type_alignment(Type type)
{
    int i;
    size_t m = 0, d;
    assert(is_object(type));

    switch (type_of(type)) {
    case T_ARRAY:
        return type_alignment(type_next(type));
    case T_STRUCT:
    case T_UNION:
        for (i = 0; i < nmembers(type); ++i) {
            d = type_alignment(get_member(type, i)->type);
            if (d > m) m = d;
        }
        assert(m);
        return m;
    default:
        return size_of(type);
    }
}

INTERNAL int nmembers(Type type)
{
    struct typetree *t = get_typetree_handle(type.ref);
    return array_len(&t->members);
}

INTERNAL struct member *get_member(Type type, int n)
{
    struct typetree *t;

    assert(n >= 0);
    t = get_typetree_handle(type.ref);
    assert(array_len(&t->members) > n);
    return &array_get(&t->members, n);
}

INTERNAL struct member *type_add_member(Type parent, String name, Type type)
{
    struct member m = {0};

    assert(is_struct_or_union(parent) || is_function(parent));
    if (!is_function(parent)) {
        m.offset = adjust_member_alignment(parent, type);
    }

    m.name = name;
    m.type = type;
    return add_member(parent, m);
}

/*
 * Attempt to place next field member right after the previous one.
 *
 *     struct {
 *         int a : 20;    // field_offset = 0, field_width = 20
 *         short b : 10;  // field_offset = 20, field_width = 10
 *         char c : 2;    // field_offset = 30, field_width = 2
 *     }
 *
 * Also pack if the next field can contain the previous field.
 *
 *     struct {
 *         short a : 10;  // field_offset = 0, field_width = 10
 *         int b : 20;    // field_offset = 10, field_width = 20
 *     }
 *
 */
static int pack_field_member(struct typetree *t, struct member *field)
{
    size_t len;
    int i, bits;
    struct member *m;
    const struct member *last;

    len = array_len(&t->members);
    if (!len) {
        return 0;
    }

    last = &array_get(&t->members, len - 1);
    if (!last->field_width) {
        return 0;
    }

    /* Check if possible to pack directly adjacent to last field. */
    bits = last->field_offset + last->field_width + field->field_width;
    if (bits <= last->field_backing) {
        field->offset = last->offset;
        field->field_offset = last->field_offset + last->field_width;
        field->field_backing = last->field_backing;
        return 1;
    }

    /* Update backing if new field has a bigger slot. */
    if (bits <= field->field_backing) {
        field->offset = last->offset;
        field->field_offset = last->field_offset + last->field_width;
        for (i = len - 1; i >= 0; --i) {
            m = &array_get(&t->members, i);
            if (m->field_width && m->offset == field->offset) {
                m->field_backing = field->field_backing;
            } else break;
        }

        return 1;
    }

    return 0;
}

/*
 * Add struct or union field member to typetree member list, updating
 * total size and alignment accordingly.
 *
 * Anonymous union fields are ignored, not needed for alignment.
 */
INTERNAL void type_add_field(Type parent, String name, Type type, size_t width)
{
    struct member m = {0};
    struct typetree *t;

    assert(is_struct_or_union(parent));
    assert(is_integer(type));

    if (width > (size_of(type) << 3) || (is_bool(type) && width > 1)) {
        error("Width of bit-field (%lu bits) exceeds width of type %t.",
            width, type);
        exit(1);
    }

    if (!str_is_empty(name) && !width) {
        error("Zero length field %s.", str_raw(name));
        exit(1);
    }

    if (is_union(parent) && str_is_empty(name)) {
        return;
    }

    m.name = name;
    m.type = type;
    m.field_width = width;
    m.field_backing = size_of(type) << 3;
    if (is_struct(parent)) {
        t = get_typetree_handle(parent.ref);
        if (!pack_field_member(t, &m)) {
            m.field_offset = 0;
            m.offset = adjust_member_alignment(parent, type);
        }
    }

    if (!width) {
        reset_field_alignment(parent, type);
    } else {
        add_member(parent, m);
    }
}

INTERNAL void type_add_anonymous_member(Type parent, Type type)
{
    int i;
    size_t offset;
    struct member m;
    struct typetree *t;

    assert(is_struct_or_union(parent));
    assert(is_struct_or_union(type));
    t = get_typetree_handle(type.ref);
    if (is_struct(parent) && is_union(type)) {
        offset = adjust_member_alignment(parent, type);
        for (i = 0; i < nmembers(type); ++i) {
            m = array_get(&t->members, i);
            m.offset += offset;
            add_member(parent, m);
        }
    } else if (is_union(parent) && is_struct(type)) {
        for (i = 0; i < nmembers(type); ++i) {
            m = array_get(&t->members, i);
            add_member(parent, m);
        }
    } else {
        for (i = 0; i < nmembers(type); ++i) {
            m = array_get(&t->members, i);
            type_add_member(parent, m.name, m.type);
        }
    }
}

/*
 * Remove anonymous field members, which are only kept for alignment
 * during type construction. Return largest remaining member alignment.
 */
static size_t remove_anonymous_fields(struct typetree *t)
{
    int i;
    size_t align, maxalign;
    struct member *m;

    maxalign = 0;
    for (i = array_len(&t->members) - 1; i >= 0; --i) {
        m = &array_get(&t->members, i);
        if (str_is_empty(m->name)) {
            array_erase(&t->members, i);
        } else {
            align = type_alignment(m->type);
            if (align > maxalign) {
                maxalign = align;
            }
        }
    }

    return maxalign;
}

/*
 * Adjust aggregate type size to be a multiple of strongest member
 * alignment.
 *
 * This function should only be called only once all members have been
 * added.
 */
INTERNAL void type_seal(Type type)
{
    struct typetree *t;
    size_t align;

    t = get_typetree_handle(type.ref);
    if (t->type == T_FUNCTION) {
        t->is_incomplete = 0;
    } else {
        assert(is_struct_or_union(type));
        align = remove_anonymous_fields(t);
        if (align == 0) {
            error("%s has no named members.",
                is_struct(type) ? "Struct" : "Union");
            exit(1);
        }

        if (t->size % align) {
            t->size += align - (t->size % align);
        }
    }
}

INTERNAL int is_vararg(Type type)
{
    struct typetree *t;

    assert(is_function(type));
    t = get_typetree_handle(type.ref);
    return t->is_vararg;
}

INTERNAL int is_vla(Type type)
{
    struct typetree *t;

    if (is_array(type)) {
        t = get_typetree_handle(type.ref);
        return t->is_vla || is_vla(t->next);
    }

    return 0;
}

INTERNAL int is_flexible(Type type)
{
    struct typetree *t;

    if (is_struct_or_union(type)) {
        t = get_typetree_handle(type.ref);
        return t->is_flexible;
    }

    return 0;
}

INTERNAL int is_variably_modified(Type type)
{
    switch (type_of(type)) {
    case T_POINTER:
        return is_variably_modified(type_next(type));
    case T_ARRAY:
        return is_vla(type);
    default: return 0;
    }
}

INTERNAL int is_complete(Type type)
{
    struct typetree *t;

    switch (type_of(type)) {
    case T_ARRAY:
    case T_FUNCTION:
        t = get_typetree_handle(type.ref);
        return !t->is_incomplete;
    default:
        return 1;
    }
}

static int typetree_equal(const struct typetree *a, const struct typetree *b)
{
    int i, len;
    struct member *ma, *mb;

    if (a->type != b->type
        || a->size != b->size
        || a->is_const != b->is_const
        || a->is_volatile != b->is_volatile
        || a->is_restrict != b->is_restrict
        || a->is_unsigned != b->is_unsigned
        || a->is_vararg != b->is_vararg
        || a->is_flexible != b->is_flexible
        || a->is_vla != b->is_vla
        || a->is_incomplete != b->is_incomplete)
    {
        return 0;
    }

    len = array_len(&a->members);
    if (len != array_len(&b->members)) {
        return 0;
    }

    if (!type_equal(a->next, b->next)) {
        return 0;
    }

    for (i = 0; i < len; ++i) {
        ma = &array_get(&a->members, i);
        mb = &array_get(&b->members, i);
        if (!type_equal(ma->type, mb->type)) {
            return 0;
        } else if (a->type != T_FUNCTION) {
            assert(ma->offset == mb->offset);
            if (!str_eq(ma->name, mb->name)) {
                return 0;
            }
        }
    }

    return 1;
}

static int typetree_equal_unqualified(
    const struct typetree *a,
    const struct typetree *b)
{
    int i, len;
    struct member *ma, *mb;

    if (a->type != b->type
        || a->size != b->size
        || a->is_unsigned != b->is_unsigned
        || a->is_vararg != b->is_vararg
        || a->is_flexible != b->is_flexible
        || a->is_vla != b->is_vla
        || a->is_incomplete != b->is_incomplete)
    {
        return 0;
    }

    len = array_len(&a->members);
    if (len != array_len(&b->members)) {
        return 0;
    }

    if (!type_equal_unqualified(a->next, b->next)) {
        return 0;
    }

    for (i = 0; i < len; ++i) {
        ma = &array_get(&a->members, i);
        mb = &array_get(&b->members, i);
        if (!type_equal_unqualified(ma->type, mb->type)) {
            return 0;
        } else if (a->type != T_FUNCTION) {
            assert(ma->offset == mb->offset);
            if (!str_eq(ma->name, mb->name)) {
                return 0;
            }
        }
    }

    return 1;
}

INTERNAL int type_equal(Type a, Type b)
{
    struct typetree *ta, *tb;
    union {
        Type type;
        struct {
            short flags, ref;
        } data;
    } x, y;

    x.type = a;
    y.type = b;

    if (x.data.flags != y.data.flags) {
        return 0;
    }

    if (x.data.ref == y.data.ref) {
        return 1;
    }

    if (x.data.ref == 0 || y.data.ref == 0) {
        return 0;
    }

    ta = get_typetree_handle(a.ref);
    tb = get_typetree_handle(b.ref);
    return typetree_equal(ta, tb);
}

INTERNAL int type_equal_unqualified(Type a, Type b)
{
    struct typetree *ta, *tb;
    union {
        Type type;
        struct {
            short flags, ref;
        } data;
    } x, y;

    x.type = type_unqualified(a);
    y.type = type_unqualified(b);

    if (x.data.flags != y.data.flags) {
        return 0;
    }

    if (x.data.ref == y.data.ref) {
        return 1;
    }

    if (x.data.ref == 0 || y.data.ref == 0) {
        return 0;
    }

    ta = get_typetree_handle(a.ref);
    tb = get_typetree_handle(b.ref);
    return typetree_equal_unqualified(ta, tb);
}


INTERNAL Type promote_integer(Type type)
{
    assert(is_integer(type));
    if (size_of(type) < 4) {
        type = basic_type__int;
    }

    return type;
}

INTERNAL Type default_argument_promotion(Type type)
{
    if (is_float(type)) {
        return basic_type__double;
    }

    if (is_integer(type)) {
        return promote_integer(type);
    }

    return type;
}

INTERNAL Type usual_arithmetic_conversion(Type t1, Type t2)
{
    Type res;

    assert(is_arithmetic(t1));
    assert(is_arithmetic(t2));
    if (is_long_double(t1) || is_long_double(t2)) {
        res = basic_type__long_double;
    } else if (is_double(t1) || is_double(t2)) {
        res = basic_type__double;
    } else if (is_float(t1) || is_float(t2)) {
        res = basic_type__float;
    } else {
        t1 = promote_integer(t1);
        t2 = promote_integer(t2);
        if (size_of(t1) > size_of(t2)) {
            res = t1;
        } else if (size_of(t2) > size_of(t1)) {
            res = t2;
        } else {
            res = is_unsigned(t1) ? t1 : t2;
        }
    }

    return type_unqualified(res);
}

INTERNAL int is_compatible(Type l, Type r)
{
    int i;
    size_t s1, s2;
    const struct member *m1, *m2;

    if (type_of(l) != type_of(r)
        || is_const(l) != is_const(r)
        || is_volatile(l) != is_volatile(r)
        || is_restrict(l) != is_restrict(r))
    {
        return 0;
    }

    switch (type_of(l)) {
    case T_BOOL:
    case T_CHAR:
    case T_SHORT:
    case T_INT:
    case T_LONG:
    case T_FLOAT:
    case T_DOUBLE:
    case T_LDOUBLE:
        return 1;
    case T_POINTER:
        return is_compatible(type_next(l), type_next(r));
    case T_ARRAY:
        s1 = type_array_len(l);
        s2 = type_array_len(r);
        /* Also accept VLA, which returns 0 length here. */
        if (s1 == 0 || s2 == 0 || s1 == s2) {
            return is_compatible(type_next(l), type_next(r));
        }
        return 0;
    case T_FUNCTION:
        if (!type_equal(type_next(l), type_next(r))) {
            return 0;
        }
        if (!is_complete(l) || !is_complete(r)) {
            return 1;
        }
        if (nmembers(l) != nmembers(r)) {
            return 0;
        }
        for (i = 0; i < nmembers(l); ++i) {
            m1 = get_member(l, i);
            m2 = get_member(r, i);
            if (!is_compatible_unqualified(m1->type, m2->type)) {
                return 0;
            }
        }
        return 1;
    default:
        return type_equal(l, r);
    }
}

INTERNAL int is_compatible_unqualified(Type l, Type r)
{
    l = type_unqualified(l);
    r = type_unqualified(r);
    return is_compatible(l, r);
}

INTERNAL size_t size_of(Type type)
{
    struct typetree *t;

    switch (type_of(type)) {
    case T_BOOL:
    case T_CHAR:
        return 1;
    case T_SHORT:
        return 2;
    case T_INT:
    case T_FLOAT:
        return 4;
    case T_LONG:
    case T_DOUBLE:
    case T_POINTER:
        return 8;
    case T_LDOUBLE:
        return 16;
    case T_STRUCT:
    case T_UNION:
        t = get_typetree_handle(type.ref);
        return t->size;
    case T_ARRAY:
        t = get_typetree_handle(type.ref);
        return t->size * size_of(t->next);
    default:
        return 0;
    }
}

INTERNAL size_t type_array_len(Type type)
{
    struct typetree *t;
    assert(is_array(type));

    t = get_typetree_handle(type.ref);
    return t->size;
}

INTERNAL const struct symbol *type_vla_length(Type type)
{
    struct typetree *t;
    assert(is_vla(type));

    t = get_typetree_handle(type.ref);
    return t->vlen;
}

INTERNAL Type type_deref(Type type)
{
    assert(is_pointer(type));
    if (type.is_pointer) {
        type = type_unqualified(type);
        type.is_pointer = 0;
    } else {
        type = get_type_handle(type.ref);
    }

    return type;
}

INTERNAL Type type_next(Type type)
{
    struct typetree *t;

    if (is_pointer(type)) {
        return type_deref(type);
    }

    assert(is_function(type) || is_array(type));
    t = get_typetree_handle(type.ref);
    return t->next;
}

INTERNAL void set_array_length(Type type, size_t length)
{
    struct typetree *t;
    assert(is_array(type));

    t = get_typetree_handle(type.ref);
    assert(!t->size);
    assert(t->is_incomplete);
    t->size = length;
    t->is_incomplete = 0;
}

INTERNAL const struct member *find_type_member(
    Type type,
    String name,
    int *index)
{
    int i;
    struct typetree *t;
    const struct member *member;

    assert(is_struct_or_union(type) || is_function(type));
    t = get_typetree_handle(type.ref);
    for (i = 0; i < array_len(&t->members); ++i) {
        member = &array_get(&t->members, i);
        if (str_eq(name, member->name)) {
            if (index) {
                *index = i;
            }
            return member;
        }
    }

    if (index) {
        *index = -1;
    }

    return NULL;
}

INTERNAL int fprinttype(FILE *stream, Type type, const struct symbol *expand)
{
    struct typetree *t;
    struct member *m;
    const char *s;
    int i, n = 0;

    if (is_const(type))
        n += fputs("const ", stream);

    if (is_volatile(type))
        n += fputs("volatile ", stream);

    if (is_restrict(type))
        n += fputs("restrict ", stream);

    if (is_unsigned(type) && !is_bool(type))
        n += fputs("unsigned ", stream);

    switch (type_of(type)) {
    case T_VOID:
        n += fputs("void", stream);
        break;
    case T_BOOL:
        n += fputs("_Bool", stream);
        break;
    case T_CHAR:
        n += fputs("char", stream);
        break;
    case T_SHORT:
        n += fputs("short", stream);
        break;
    case T_INT:
        n += fputs("int", stream);
        break;
    case T_LONG:
        n += fputs("long", stream);
        break;
    case T_FLOAT:
        n += fputs("float", stream);
        break;
    case T_DOUBLE:
        n += fputs("double", stream);
        break;
    case T_LDOUBLE:
        n += fputs("long double", stream);
        break;
    case T_POINTER:
        n += fputs("* ", stream);
        n += fprinttype(stream, type_deref(type), NULL);
        break;
    case T_FUNCTION:
        t = get_typetree_handle(type.ref);
        n += fputs("(", stream);
        if (is_complete(type) && nmembers(type) == 0) {
            n += fputs("void", stream);
        } else {
            for (i = 0; i < nmembers(type); ++i) {
                m = get_member(type, i);
                if (m->offset) {
                    assert(is_pointer(m->type));
                    fprintf(stream, "static(%lu) ", m->offset);
                }
                n += fprinttype(stream, m->type, NULL);
                if (i < nmembers(type) - 1) {
                    n += fputs(", ", stream);
                }
            }
            if (is_vararg(type)) {
                n += fputs(", ...", stream);
            }
        }
        n += fputs(") -> ", stream);
        n += fprinttype(stream, t->next, NULL);
        break;
    case T_ARRAY:
        t = get_typetree_handle(type.ref);
        if (t->is_vla) {
            if (t->vlen) {
                n += fprintf(stream, "[%s] ", sym_name(t->vlen));
            } else {
                n += fputs("[*] ", stream);
            }
        } else if (t->is_incomplete) {
            n += fputs("[] ", stream);
        } else {
            n += fprintf(stream, "[%lu] ", t->size);
        }
        n += fprinttype(stream, t->next, NULL);
        break;
    case T_STRUCT:
    case T_UNION:
        t = get_typetree_handle(type.ref);
        if (t->tag && (t->tag != expand)) {
            if (t->tag->symtype == SYM_TAG) {
                s = is_union(type) ? "union" : "struct";
                n += fprintf(stream, "%s %s", s, sym_name(t->tag));
            } else {
                assert(t->tag->symtype == SYM_TYPEDEF);
                n += fprintf(stream, "%s", sym_name(t->tag));
            }
        } else {
            n += fputc('{', stream);
            for (i = 0; i < nmembers(type); ++i) {
                m = get_member(type, i);
                n += fprintf(stream, ".%s::", str_raw(m->name));
                n += fprinttype(stream, m->type, NULL);
                if (m->field_width) {
                    n += fprintf(stream, " (+%lu:%d:%d):%d",
                        m->offset, m->field_offset, m->field_width,
                        m->field_backing);
                } else {
                    n += fprintf(stream, " (+%lu)", m->offset);
                }
                if (i < nmembers(type) - 1) {
                    n += fputs(", ", stream);
                }
            }
            n += fputc('}', stream);
        }
        break;
    }

    return n;
}
