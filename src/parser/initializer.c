#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target);

static struct block *read_initializer_element(
    struct definition *def,
    struct block *block,
    struct var target)
{
    int ops;
    struct var value;

    ops = array_len(&block->code);
    block = assignment_expression(def, block);
    value = block->expr.l;
    if (target.symbol->linkage != LINK_NONE
        && (array_len(&block->code) - ops > 0
            || !is_identity(block->expr)
            || (!is_constant(value) && value.kind != ADDRESS
                && !(value.kind == DIRECT && is_function(value.type)))
            || (value.kind == ADDRESS
                && value.symbol->linkage == LINK_NONE)))
    {
        error("Initializer must be computable at load time.");
        exit(1);
    }

    return block;
}

static int next_element(void)
{
    struct token t = peek();
    if (t.token == ',') {
        if (peekn(2).token != '}') {
            next();
            return 1;
        }
    }

    return 0;
}

/*
 * Initialize members of a struct or union.
 *
 * Only the first element of a union can be initialized. If the first
 * element is not also the largest member, or if there is padding, the
 * remaining memory is undefined.
 *
 * Members of structs can have overlapping offsets from anonymous union
 * fields. Act as if only the first element is initialized by skipping
 * all consecutive elements with the same offset.
 */
static struct block *initialize_struct_or_union(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    int i, m;
    size_t filled;
    Type type;
    const struct member *member, *prev;

    target.lvalue = 1;
    filled = target.offset;
    type = target.type;
    prev = NULL;
    assert(is_struct_or_union(type));
    assert(nmembers(type) > 0);

    if (is_union(type)) {
        member = get_member(type, 0);
        target.type = member->type;
        target.field_offset = member->field_offset;
        target.field_width = member->field_width;
        block = initialize_member(def, block, values, target);
    } else {
        m = nmembers(type);
        i = 0;
        do {
            while (1) {
                member = get_member(type, i++);
                if (!prev
                    || prev->offset != member->offset
                    || prev->field_offset != member->field_offset)
                    break;
            }
            target.type = member->type;
            prev = member;
            target.offset = filled + member->offset;
            target.field_offset = member->field_offset;
            target.field_width = member->field_width;
            block = initialize_member(def, block, values, target);
        } while (i < m && next_element());
    }

    return block;
}

static int is_string(struct expression expr)
{
    return is_identity(expr)
        && expr.l.kind == IMMEDIATE && expr.l.symbol
        && expr.l.symbol->symtype == SYM_STRING_VALUE;
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
    struct var target)
{
    Type type = target.type;

    assert(is_array(type));
    assert(target.kind == DIRECT);
    if (is_aggregate(type_next(target.type))) {
        target.type = type_next(target.type);
        do {
            block = initialize_member(def, block, values, target);
            target.offset += size_of(target.type);
        } while (next_element());
    } else {
        block = read_initializer_element(def, block, target);
        if (is_char(type_next(target.type)) && is_string(block->expr)) {
            target = eval_assign(def, values, target, block->expr);
        } else {
            target.type = type_next(target.type);
            eval_assign(def, values, target, block->expr);
            target.offset += size_of(target.type);
            while (next_element()) {
                block = read_initializer_element(def, block, target);
                eval_assign(def, values, target, block->expr);
                target.offset += size_of(target.type);
            }
        }
    }

    if (!size_of(type)) {
        assert(is_array(target.symbol->type));
        assert(!size_of(target.symbol->type));
        type_set_array_size(target.symbol->type, target.offset);
    }

    return block;
}

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    assert(target.kind == DIRECT);
    if (is_struct_or_union(target.type)) {
        if (peek().token == '{') {
            next();
            block = initialize_struct_or_union(def, block, values, target);
            if (peek().token == ',')
                next();
            consume('}');
        } else {
            block = initialize_struct_or_union(def, block, values, target);
        }
    } else if (is_array(target.type)) {
        if (!size_of(target.type)) {
            error("Invalid initialization of flexible array member.");
            exit(1);
        }
        if (peek().token == '{') {
            next();
            block = initialize_array(def, block, values, target);
            if (peek().token == ',')
                next();
            consume('}');
        } else {
            block = initialize_array(def, block, values, target);
        }
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, values, target, block->expr);
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

    if (peek().token == '{') {
        next();
        if (is_struct_or_union(target.type)) {
            block = initialize_struct_or_union(def, block, values, target);
        } else if (is_array(target.type)) {
            block = initialize_array(def, block, values, target);
        } else {
            block = initialize_object(def, block, values, target);
        }
        if (peek().token == ',') {
            next();
        }
        consume('}');
    } else if (is_array(target.type)) {
        block = initialize_array(def, block, values, target);
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, values, target, block->expr);
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
    size = size_of(target.type);
    switch (type_of(target.type)) {
    case T_STRUCT:
    case T_UNION:
        assert(size);
        target.type = (size % 8)
            ? type_create(T_ARRAY, basic_type__char, size, NULL)
            : type_create(T_ARRAY, basic_type__long, size / 8, NULL);
    case T_ARRAY:
        var = target;
        target.type = type_next(target.type);
        for (i = 0; i < size / size_of(target.type); ++i) {
            target.offset = var.offset + i * size_of(target.type);
            zero_initialize(def, values, target);
        }
        break;
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
 * Zero initialize padding bytes between target reference and next field
 * assignment.
 *
 * Target has offset and field offset pointing to first location not yet
 * initialized.
 */
static void initialize_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    struct var field)
{
    size_t padding;

    if (target.offset < field.offset) {
        if (target.field_offset) {
            target.type = basic_type__int;
            target.field_width = 32 - target.field_offset;
            zero_initialize(def, block, target);
            target.offset += size_of(target.type);
        }
        padding = field.offset - target.offset;
        zero_initialize_bytes(def, block, target, padding);
    } else if (target.field_offset < field.field_offset) {
        target.type = basic_type__int;
        target.field_width = field.field_offset - target.field_offset;
        zero_initialize(def, block, target);
    }
}

/*
 * Initialize padding at the end of object.
 *
 * Consider both last bits of a bitfield, and any remaining space after
 * the bitfield itself.
 */
static void initialize_trailing_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    size_t size)
{
    if (target.field_offset) {
        target.type = basic_type__int;
        target.field_width = 32 - target.field_offset;
        zero_initialize(def, block, target);
        target.offset += size_of(target.type);
    }

    if (size > target.offset) {
        zero_initialize_bytes(def, block, target, size - target.offset);
    }
}

/*
 * Evaluate sequence of IR_ASSIGN statements in a separate block. This
 * is appended at the end after all expressions inside initializers are
 * evaluated.
 *
 * Padding initialization is handled only after the whole initializer is
 * read, as postprocessing of the statements in this block.
 */
static struct block *get_initializer_block(int i)
{
    static struct block *block[2];

    return !block[i] ? (block[i] = cfg_block_init(NULL)) : block[i];
}

#ifndef NDEBUG
static void validate_initializer_block(struct block *block)
{
    int i;
    struct statement st;
    struct var target = {0}, field;

    for (i = 0; i < array_len(&block->code); ++i) {
        st = array_get(&block->code, i);
        assert(st.st == IR_ASSIGN);
        assert(target.offset <= st.t.offset);
        field = st.t;
        if (target.offset < field.offset) {
            assert(field.offset - target.offset == size_of(target.type));
        } else {
            assert(target.offset == field.offset);
            assert(target.field_offset + target.field_width
                == field.field_offset);
        }

        target = field;
    }
}
#endif

/*
 * Fill in any missing padding initialization in assignment statment
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
    int i;
    struct statement st;
    struct var field;
    struct block *block = get_initializer_block(1);

    for (i = 0; i < array_len(&values->code); ++i) {
        st = array_get(&values->code, i);
        assert(st.st == IR_ASSIGN);
        assert(target.offset <= st.t.offset);
        field = st.t;

        initialize_padding(def, block, target, field);

        array_push_back(&block->code, st);
        target.offset = field.offset;
        if (field.field_width) {
            target.field_offset = field.field_offset + field.field_width;
            if (target.field_offset == 32) {
                target.field_offset = 0;
                target.field_width = 0;
                target.offset += size_of(field.type);
            }
        } else {
            target.field_offset = 0;
            target.field_width = 0;
            target.offset += size_of(field.type);
        }
    }

    initialize_trailing_padding(def, block, target, size_of(target.type));
    array_empty(&values->code);
#ifndef NDEBUG
    validate_initializer_block(block);
#endif
    return block;
}

struct block *initializer(
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    struct block *values = get_initializer_block(0);
    struct var target = var_direct(sym);

    if (peek().token == '{' || is_array(sym->type)) {
        block = initialize_object(def, block, values, target);
        values = postprocess_object_initialization(def, values, target);
        array_concat(&block->code, &values->code);
        array_empty(&values->code);
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, block, target, block->expr);
    }

    return block;
}
