#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static const struct var var__immediate_zero = {IMMEDIATE, {T_INT}};

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target);

static int is_string(struct expression expr)
{
    return is_identity(expr)
        && expr.l.kind == IMMEDIATE && expr.l.symbol
        && expr.l.symbol->symtype == SYM_STRING_VALUE;
}

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
 * Zero initialize padding bytes between previous and current member.
 *
 * Offset and field offset written so far is stored in target.
 */
static void initialize_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    const struct member *prev,
    const struct member *member)
{
    size_t padding;

    if (!prev) {
        if (member->field_offset) {
            target.field_offset = 0;
            target.field_width = member->field_offset;
            target.type = basic_type__int;
            zero_initialize(def, block, target);
        }
    } else if (prev->offset == member->offset) {
        if (prev->field_offset + prev->field_width < member->field_offset) {
            target.field_offset = prev->field_offset + prev->field_width;
            target.field_width = member->field_offset - target.field_offset;
            target.type = basic_type__int;
            zero_initialize(def, block, target);
        }
    } else if (prev->offset + size_of(prev->type) < member->offset) {
        padding = member->offset - (prev->offset + size_of(prev->type));
        target.offset += size_of(prev->type);
        zero_initialize_bytes(def, block, target, padding);
    }
}

/*
 * Initialize padding at the end of a struct.
 *
 * Consider both last bits of a bitfield, and any remaining space after
 * the bitfield itself.
 */
static void initialize_trailing_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    Type type)
{
    size_t padding;
    const struct member *member;

    member = get_member(type, nmembers(type) - 1);
    if (member->field_width
        && member->field_offset + member->field_width < 32)
    {
        target.type = basic_type__int;
        target.field_offset = member->field_offset + member->field_width;
        target.field_width = 32 - target.field_offset;
        zero_initialize(def, block, target);
    }

    if (member->offset + size_of(member->type) < size_of(type)) {
        padding = size_of(type) - (member->offset + size_of(member->type));
        target.offset += size_of(member->type);
        zero_initialize_bytes(def, block, target, padding);
    }
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
            initialize_padding(def, values, target, prev, member);
            target.type = member->type;
            prev = member;
            target.offset = filled + member->offset;
            target.field_offset = member->field_offset;
            target.field_width = member->field_width;
            block = initialize_member(def, block, values, target);
        } while (i < m && next_element());

        while (i < m) {
            member = get_member(type, i++);
            initialize_padding(def, values, target, prev, member);
            target.type = member->type;
            prev = member;
            target.offset = filled + member->offset;
            target.field_offset = member->field_offset;
            target.field_width = member->field_width;
            zero_initialize(def, values, target);
        }

        initialize_trailing_padding(def, values, target, type);
    }

    return block;
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
 * This will generates the following IR assignments:
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
    size_t filled = target.offset;

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
    } else {
        target.type = type_next(type);
        while (target.offset - filled < size_of(type)) {
            zero_initialize(def, values, target);
            target.offset += size_of(target.type);
        }
    }

    return block;
}

/* Initialize member of an aggregate type. */
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

/*
 * Evaluate sequence of IR_ASSIGN statements in a separate block. This
 * is appended at the end after all expressions inside initializers are
 * evaluated.
 */
static struct block *get_initializer_block()
{
    static struct block *block;

    return !block ? (block = cfg_block_init(NULL)) : block;
}

struct block *initializer(
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    struct block *values = get_initializer_block();
    struct var target = var_direct(sym);

    if (peek().token == '{' || is_array(sym->type)) {
        block = initialize_object(def, block, values, target);
        array_concat(&block->code, &values->code);
        array_empty(&values->code);
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, block, target, block->expr);
    }

    return block;
}
