#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

/*
 * Introduce separate blocks to hold list of assignment operations for
 * each initializer. This is appended at the end after all expressions
 * inside initializers are evaluated.
 *
 * Padding initialization is handled only after the whole initializer is
 * read, as postprocessing of the statements in these blocks.
 *
 * Since initializers can be nested with compound literals, we need
 * arbitrary many blocks. Re-use for memory efficiency.
 */
static array_of(struct block *) inititializer_blocks;

static struct block *get_initializer_block(void)
{
    struct block *block;

    if (array_len(&inititializer_blocks)) {
        block = array_pop_back(&inititializer_blocks);
    } else {
        block = cfg_block_init(NULL);
    }

    return block;
}

static void release_initializer_block(struct block *block)
{
    assert(!block->label);
    assert(!block->has_init_value);

    array_empty(&block->code);
    array_push_back(&inititializer_blocks, block);
}

INTERNAL void initializer_finalize(void)
{
    array_clear(&inititializer_blocks);
}

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target);

static int is_loadtime_constant(struct expression expr)
{
    if (!is_identity(expr))
        return 0;

    switch (expr.l.kind) {
    case IMMEDIATE:
        return 1;
    case DIRECT:
        if (!is_array(expr.type) && !is_function(expr.type))
            return 0;
    case ADDRESS:
        return expr.l.symbol->linkage != LINK_NONE;
    default:
        return 0;
    }
}

/*
 * Read assignment expression into block->expr.
 *
 * Since initializer assignments can be reordered, we need to evaluate
 * call expressions into a temporary variable.
 */
static struct block *read_initializer_element(
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    size_t ops;
    struct var tmp;
    const struct block *top;

    assert(!block->has_init_value);
    ops = array_len(&block->code);
    top = block;
    block = assignment_expression(def, block);
    if (is_void(block->expr.type)) {
        error("Cannot initialize with void value.");
        exit(1);
    }

    if (sym->linkage != LINK_NONE) {
        if (block != top
            || array_len(&block->code) - ops > 0
            || !is_identity(block->expr)
            || !is_loadtime_constant(block->expr))
        {
            error("Initializer must be computable at load time.");
            exit(1);
        }
    } else if (block->expr.op == IR_OP_CALL) {
        tmp = create_var(def, block->expr.type);
        eval_assign(def, block, tmp, block->expr);
        block->expr = as_expr(tmp);
    }

    block->has_init_value = 1;
    return block;
}

enum current_object_state {
    CURRENT,        /* Current object. */
    DESIGNATOR,     /* In designator. */
    MEMBER
};

static int next_element(enum current_object_state state)
{
    if (peek() == ',') {
        switch (peekn(2)) {
        case '}':
            break;
        case '.':
            if (state != CURRENT) {
                break;
            }
        default:
            next();
            return 1;
        }
    }

    return 0;
}

static struct var access_member(
    struct var target,
    const struct member *member,
    size_t offset)
{
    target.type = member->type;
    target.field_offset = member->field_offset;
    target.field_width = member->field_width;
    target.offset = offset + member->offset;
    return target;
}

static const struct member *get_named_member(
    Type type,
    String name,
    int *i)
{
    const struct member *member;

    member = find_type_member(type, name, i);
    if (member == NULL) {
        error("%t has no member named %s.", type, str_raw(name));
        exit(1);
    }

    return member;
}

/*
 * Initialize the first union member, or the last member specified by a
 * designator.
 *
 * If the initialized element is not also the largest member, or if
 * there is padding, the remaining memory is undefined.
 *
 * With designators, there can be arbitrary many member initializers,
 * but only the last one should count. Evaluate each member in its own
 * block to cleanly reset.
 *
 *     union {
 *         struct { int x, y; } p;
 *         int q;
 *     } foo = {{1, 2}, .q = 3};
 *
 * In the above definition, we want the value of foo.p.y to be 0, even
 * though the assignment to .q does not overwrite it.
 */
static struct block *initialize_union(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    int done;
    size_t filled;
    struct block *init;
    const struct member *member;
    Type type;
    String name;

    done = 0;
    filled = target.offset;
    type = target.type;
    init = get_initializer_block();
    assert(is_union(type));
    assert(nmembers(type) > 0);

    do {
        if (try_consume('.')) {
            consume(IDENTIFIER);
            name = access_token(0)->d.string;
            member = get_named_member(type, name, NULL);
            target = access_member(target, member, filled);
            if (peek() == '=') {
                next();
            }
        } else if (!done) {
            member = get_member(type, 0);
            target = access_member(target, member, filled);
        } else break;
        array_empty(&init->code);
        block = initialize_member(def, block, init, target);
        done = 1;
    } while (next_element(state));

    array_concat(&values->code, &init->code);
    release_initializer_block(init);
    return block;
}

/*
 * Initialize members of a struct.
 *
 * Members of structs can have overlapping offsets from anonymous union
 * fields. Act as if only the first element is initialized by skipping
 * all consecutive elements with the same offset.
 */
static struct block *initialize_struct(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    int i, m;
    size_t filled;
    Type type;
    String name;
    const struct member *member, *prev;

    prev = NULL;
    target.lvalue = 1;
    filled = target.offset;
    type = target.type;
    assert(is_struct(type));
    assert(nmembers(type) > 0);

    m = nmembers(type);
    i = 0;

    do {
        if (!block->has_init_value && try_consume('.')) {
            consume(IDENTIFIER);
            name = access_token(0)->d.string;
            member = get_named_member(type, name, &i);
            target = access_member(target, member, filled);
            if (peek() == '=') {
                next();
            }
            block = initialize_member(def, block, values, target);
            prev = member;
            i += 1;
        } else {
            while (1) {
                member = get_member(type, i);
                i += 1;
                if (!prev
                    || prev->offset != member->offset
                    || prev->field_offset != member->field_offset)
                    break;
            }
            prev = member;
            target = access_member(target, member, filled);
            block = initialize_member(def, block, values, target);
            if (i >= m)
                break;
        }
    } while (next_element(state));

    return block;
}

/*
 * Read initializer for struct or union. Make sure to read first element
 * if possible, to catch assignments of aggregate values initializing
 * the whole object at once.
 *
 *     struct A { char c; } foo = { 'a' };
 *     struct { struct A a; } bar = { foo };
 *
 */
static struct block *initialize_struct_or_union(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    assert(is_struct_or_union(target.type));
    assert(nmembers(target.type) > 0);

    if (!block->has_init_value) switch (peek()) {
    case '.':
    case '{':
    case '[':
        break;
    default:
        block = read_initializer_element(def, block, target.symbol);
        break;
    }

    if (block->has_init_value
        && is_compatible_unqualified(target.type, block->expr.type))
    {
        eval_assign(def, values, target, block->expr);
        block->has_init_value = 0;
    } else if (is_union(target.type)) {
        block = initialize_union(def, block, values, target, state);
    } else {
        block = initialize_struct(def, block, values, target, state);
    }

    return block;
}

static int has_next_array_element(
    enum current_object_state state,
    int *is_designator)
{
    *is_designator = 0;
    if (peek() == ',') {
        switch (peekn(2)) {
        case '}':
        case '.':
            break;
        case '[':
            if (state != CURRENT) {
                break;
            }
            *is_designator = 1;
        default:
            return 1;
        }
    }

    return 0;
}

static int try_parse_index(size_t *index)
{
    struct var num;

    if (try_consume('[')) {
        num = constant_expression();
        if (!is_integer(num.type)) {
            error("Array designator must have integer value.");
            exit(1);
        }

        consume(']');
        *index = num.imm.i;
        return 1;
    }

    return 0;
}

/*
 * Initialize array types with brace-enclosed values, or string literal.
 *
 *     a[] = {1, 2, 3};
 *     b[] = "Hello world"
 *     c[2][3] = {1, 2, 3, {4, 5, 6}}
 *
 * Handle special case of incomplete array type, and assignment to
 * arrays which are longer than the string itself. In that case, the
 * rest of the array is initialized to zero.
 *
 *      char foo[5] = "Hi"
 *
 * This will generates the following IR assignments, after padding is
 * added at the end:
 *
 *      foo = "Hi"
 *      foo[3] = 0
 *      foo[4] = 0
 */
static struct block *initialize_array(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    int is_designator;
    Type type, elem;
    size_t initial, width, count, i, c;

    assert(is_array(target.type));
    assert(target.kind == DIRECT);

    i = c = 0;
    count = type_array_len(target.type);
    type = target.type;
    elem = type_next(type);
    width = size_of(elem);
    initial = target.offset;

    /*
     * Need to read expression to determine if element is a string
     * constant, or an integer like "Hello"[2].
     */
    if (!block->has_init_value) switch (peek()) {
    case '.':
    case '{':
    case '[':
        break;
    default:
        block = read_initializer_element(def, block, target.symbol);
        break;
    }

    /* Assign string literal to initialize the whole array. */
    if (block->has_init_value
        && is_char(elem)
        && is_identity(block->expr)
        && is_array(block->expr.type)
        && block->expr.l.kind == DIRECT
        && block->expr.l.symbol->symtype == SYM_LITERAL)
    {
        target = eval_assign(def, values, target, block->expr);
        block->has_init_value = 0;
    } else {
        target.type = elem;
        while (1) {
            if (try_parse_index(&i) && peek() == '=') {
                next();
            }

            target.offset = initial + (i * width);
            block = initialize_member(def, block, values, target);
            i += 1;
            c = i > c ? i : c;
            if (has_next_array_element(state, &is_designator)) {
                if (!is_designator && count && c >= count)
                    break;
                consume(',');
            } else break;
        }
    }

    if (!size_of(type)) {
        assert(is_array(target.symbol->type));
        assert(!size_of(target.symbol->type));
        set_array_length(target.symbol->type, c);
    }

    return block;
}

/*
 * Add assignment operation to initializer values block.
 *
 * Assignment evaluation can generate a cast statement, which needs to
 * be added to the normal block.
 */
static void assign_initializer_element(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    struct statement st;
    assert(target.kind == DIRECT);
    assert(block->has_init_value);

    eval_assign(def, block, target, block->expr);
    st = array_pop_back(&block->code);
    assert(st.st == IR_ASSIGN);
    array_push_back(&values->code, st);
    block->has_init_value = 0;
}

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    assert(target.kind == DIRECT);

    if (is_struct_or_union(target.type)) {
        if (!block->has_init_value && try_consume('{')) {
            block = initialize_struct_or_union(def, block, values, target, CURRENT);
            try_consume(',');
            consume('}');
        } else {
            block = initialize_struct_or_union(def, block, values, target, DESIGNATOR);
        }
    } else if (is_array(target.type)) {
        if (!size_of(target.type)) {
            error("Invalid initialization of flexible array member.");
            exit(1);
        }
        if (!block->has_init_value && try_consume('{')) {
            block = initialize_array(def, block, values, target, CURRENT);
            try_consume(',');
            consume('}');
        } else {
            block = initialize_array(def, block, values, target, DESIGNATOR);
        }
    } else {
        if (!block->has_init_value) {
            if (try_consume('{')) {
                block = read_initializer_element(def, block, target.symbol);
                consume('}');
            } else {
                block = read_initializer_element(def, block, target.symbol);
            }
        }

        assign_initializer_element(def, block, values, target);
    }

    return block;
}

static struct block *initialize_object(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    assert(target.kind == DIRECT);
    assert(!block->has_init_value);

    if (try_consume('{')) {
        if (is_struct_or_union(target.type)) {
            block = initialize_struct_or_union(def, block, values, target, CURRENT);
        } else if (is_array(target.type)) {
            block = initialize_array(def, block, values, target, CURRENT);
        } else {
            block = initialize_object(def, block, values, target);
        }

        try_consume(',');
        consume('}');
    } else if (is_array(target.type)) {
        block = initialize_array(def, block, values, target, MEMBER);
    } else {
        block = read_initializer_element(def, block, target.symbol);
        assign_initializer_element(def, block, values, target);
    }

    return block;
}

static const struct var var__immediate_zero = {IMMEDIATE, {T_INT}};

/*
 * Set var = 0, using simple assignment on members for composite types.
 *
 * This rule does not consume any input, but generates a series of
 * assignments on the given variable. Point is to be able to zero
 * initialize using normal simple assignment rules, although IR can
 * become verbose for large structures.
 */
static void zero_initialize(
    struct definition *def,
    struct block *values,
    struct var target)
{
    int i;
    size_t size;
    struct var var;

    assert(target.kind == DIRECT);
    assert(!values->has_init_value);
    size = size_of(target.type);
    switch (type_of(target.type)) {
    case T_STRUCT:
    case T_UNION:
        assert(size);
        target.type = (size % 8)
            ? type_create_array(basic_type__char, size)
            : type_create_array(basic_type__long, size / 8);
    case T_ARRAY:
        var = target;
        target.type = type_next(target.type);
        for (i = 0; i < size / size_of(target.type); ++i) {
            target.offset = var.offset + i * size_of(target.type);
            zero_initialize(def, values, target);
        }
        break;
    case T_BOOL:
    case T_CHAR:
    case T_SHORT:
    case T_INT:
    case T_LONG:
    case T_FLOAT:
    case T_DOUBLE:
    case T_LDOUBLE:
    case T_POINTER:
        var = var__immediate_zero;
        var.type = target.type;
        eval_assign(def, values, target, as_expr(var));
        break;
    default:
        error("Cannot zero-initialize object of type '%t'.", target.type);
        exit(1);
    }
}

static void zero_initialize_bytes(
    struct definition *def,
    struct block *values,
    struct var target,
    size_t bytes)
{
    size_t size;

    target.field_offset = 0;
    target.field_width = 0;
    while (bytes) {
        size = bytes % 8;
        if (!size) {
            size = 8;
        }

        assert(size <= bytes);
        switch (size) {
        default:
            size = 1;
        case 1:
            target.type = basic_type__char;
            break;
        case 2:
            target.type = basic_type__short;
            break;
        case 4:
            target.type = basic_type__int;
            break;
        case 8:
            target.type = basic_type__long;
            break;
        }

        zero_initialize(def, values, target);
        target.offset += size_of(target.type);
        bytes -= size;
    }
}

/*
 * Zero initialize padding bytes between previous and next field
 * assignment.
 *
 * Previous member has offset and field offset pointing to first
 * location not yet initialized. All other properties are undefined.
 *
 * To make IR logical, first try to complete bitfield, then add bytes
 * as needed, and end with potential padding as start of new bitfield.
 */
static void initialize_padding(
    struct definition *def,
    struct block *block,
    struct var prev,
    struct var next)
{
    size_t bytes;

    assert(!prev.field_width);
    assert(prev.offset <= next.offset);
    assert(prev.offset * 8 + prev.field_offset
        <= next.offset * 8 + next.field_offset);

    while (1) {
        if (prev.offset == next.offset) {
            assert(prev.field_offset <= next.field_offset);
            prev.field_width = next.field_offset - prev.field_offset;
            if (prev.field_width) {
                if (next.field_offset > 32) {
                    assert(next.field_offset < 64);
                    prev.type = basic_type__unsigned_long;
                } else if (next.field_offset > 16) {
                    prev.type = basic_type__unsigned_int;
                } else if (next.field_offset > 8) {
                    prev.type = basic_type__unsigned_short;
                } else {
                    prev.type = basic_type__unsigned_char;
                }

                zero_initialize(def, block, prev);
            }

            break;
        }

        switch (prev.field_offset) {
        case 8:
        case 16:
        case 32:
        case 64:
            prev.offset += prev.field_offset / 8;
            prev.field_offset = 0;
        case 0:
            break;
        default:
            if (prev.field_offset < 32) {
                prev.type = basic_type__unsigned_int;
            } else {
                assert(prev.field_offset < 64);
                prev.type = basic_type__unsigned_long;
            }

            bytes = prev.offset + size_of(prev.type);
            prev.field_width = size_of(prev.type) * 8 - prev.field_offset;
            if (bytes > next.offset) {
                assert(prev.field_width * 8 > (bytes - next.offset));
                prev.field_width -= (bytes - next.offset) * 8;
            }

            assert((prev.field_offset + prev.field_width) % 8 == 0);
            zero_initialize(def, block, prev);
            prev.offset += (prev.field_offset + prev.field_width) / 8;
            prev.field_offset = 0;
            prev.field_width = 0;
            break;
        }

        assert(prev.offset <= next.offset);
        zero_initialize_bytes(def, block, prev, next.offset - prev.offset);
        prev.offset = next.offset;
    }
}

static int is_constant_assignment(const struct statement *st)
{
    assert(st->st == IR_ASSIGN);
    return is_identity(st->expr)
        && is_integer(st->expr.type)
        && st->expr.l.kind == IMMEDIATE;
}

static int merge_assignments(struct statement *a, const struct statement *b)
{
    long m1, m2;

    if (a->t.offset != b->t.offset
        || !is_constant_assignment(a)
        || !is_constant_assignment(b))
    {
        return 0;
    }

    assert(is_field(a->t) && is_field(b->t));

    m1 = ((1l << a->t.field_width) - 1);
    m2 = ((1l << b->t.field_width) - 1);

    a->t.type = usual_arithmetic_conversion(a->t.type, b->t.type);
    a->t.field_width += b->t.field_width;
    a->expr.type = a->t.type;
    a->expr.l.imm.i = ((a->expr.l.imm.i & m1) << a->t.field_offset)
        | ((b->expr.l.imm.i & m2) << b->t.field_offset);
    a->expr.l.type = a->t.type;
    a->expr.l.symbol = NULL;
    if (!a->t.field_offset && a->t.field_width == size_of(a->t.type) * 8) {
        a->t.field_width = 0;
    }

    return 1;
}

/*
 * Merge adjacent field assignments with constant values.
 *
 * This is a required step for backend to be able to emit correct code,
 * since it can enforce more restrictions on the IR. Static or global
 * variables will always be assigned as whole bytes.
 */
static void normalize_field_assignment(
    struct definition *def,
    struct block *block)
{
    int i;
    struct statement *a, *b;

    assert(array_len(&block->code) > 1);
    a = &array_get(&block->code, 0);

    for (i = 1; i < array_len(&block->code); ++i) {
        b = &array_get(&block->code, i);
        if (merge_assignments(a, b)) {
            array_erase(&block->code, i);
            i--;
        } else {
            a = b;
        }
    }
}

#ifndef NDEBUG

/*
 * Initializer blocks should always result in a list of assignment
 * operations writing to all bits of the target object, in order.
 *
 * Some additional constrants are put on field assignments; the first
 * assignment to a field on a new offset must have field_offset 0.
 */
static size_t validate_contiguous_initialization(struct block *block)
{
    int i;
    size_t bits = 0;
    struct statement st;
    struct var field, prev;

    for (i = 0; i < array_len(&block->code); ++i) {
        st = array_get(&block->code, i);
        assert(st.st == IR_ASSIGN);
        field = st.t;

        if (field.field_width) {
            assert(!field.field_offset
                || (i && prev.offset == field.offset));
            assert(field.offset * 8 + field.field_offset == bits);
            bits += field.field_width;
        } else {
            assert(field.offset * 8 == bits);
            bits += size_of(field.type) * 8;
        }

        prev = field;
    }

    assert(bits % 8 == 0);
    return bits / 8;
}

#endif

/*
 * Reorder initializer assignments to increasing offsets, and remove
 * duplicate assignments to the same element.
 */
static void sort_and_trim(struct block *values)
{
    int i, j;
    struct statement *code, tmp;

    code = &array_get(&values->code, 0);
    for (i = 1; i < array_len(&values->code); ++i) {
        j = i - 1;
        while (j >= 0 && code[j].t.offset > code[j + 1].t.offset) {
            tmp = code[j];
            code[j] = code[j + 1];
            code[j + 1] = tmp;
            if (j == 0) {
                break;
            } else {
                j--;
            }
        }

        if (code[j].t.offset == code[j + 1].t.offset
            && code[j].t.field_offset == code[j + 1].t.field_offset)
        {
            assert(code[j].t.field_width == code[j + 1].t.field_width);
            array_erase(&values->code, j);
            i -= 1;
        }
    }
}

/*
 * Fill in any missing padding initialization in assignment statement
 * list.
 *
 * The input block contains a list of assignments to the same variable,
 * possibly sparsely covering the full size of the type.
 */
static struct block *postprocess_object_initialization(
    struct definition *def,
    struct block *values,
    struct var target)
{
    int i, has_field;
    struct statement st;
    struct var prev, next;
    struct block *block;

    assert(!target.offset);
    sort_and_trim(values);
    block = get_initializer_block();
    prev = target;

    for (i = 0, has_field = 0; i < array_len(&values->code); ++i) {
        st = array_get(&values->code, i);
        next = st.t;
        assert(st.st == IR_ASSIGN);
        assert(st.expr.op != IR_OP_CALL);
        assert(next.symbol == target.symbol);
        initialize_padding(def, block, prev, next);
        array_push_back(&block->code, st);
        prev.offset = next.offset;
        prev.field_offset = next.field_offset + next.field_width;
        if (!next.field_width) {
            prev.offset += size_of(next.type);
        } else {
            has_field = 1;
        }
    }

    next.offset = size_of(target.type);
    next.field_offset = 0;
    initialize_padding(def, block, prev, next);
    if (has_field) {
        normalize_field_assignment(def, block);
    }

    release_initializer_block(values);
    assert(validate_contiguous_initialization(block) == size_of(target.type));
    return block;
}

INTERNAL struct block *initializer(
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    struct block *values;
    struct var target = var_direct(sym);

    if (peek() == '{' || is_array(sym->type)) {
        values = get_initializer_block();
        block = initialize_object(def, block, values, target);
        values = postprocess_object_initialization(def, values, target);
        array_concat(&block->code, &values->code);
        release_initializer_block(values);
    } else {
        block = read_initializer_element(def, block, target.symbol);
        eval_assign(def, block, target, block->expr);
        block->has_init_value = 0;
    }

    assert(!block->has_init_value);
    return block;
}
