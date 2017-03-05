#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "statement.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *initializer(
    struct definition *def,
    struct block *block,
    struct var target);

static const Type *get_typedef(String str)
{
    struct symbol *tag;

    tag = sym_lookup(&ns_ident, str);
    if (tag && tag->symtype == SYM_TYPEDEF) {
        return &tag->type;
    }

    return NULL;
}

static int is_type_placeholder(Type type)
{
    return type.type == -1;
}

static Type get_type_placeholder(void)
{
    Type t = {-1};
    return t;
}

/*
 * FOLLOW(parameter-list) = { ')' }, peek to return empty list; even
 * though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static Type parameter_list(Type base)
{
    String name;
    Type func, type;
    struct symbol *sym;

    func = type_create(T_FUNCTION, base);
    while (peek().token != ')') {
        name.len = 0;
        type = declaration_specifiers(NULL, NULL);
        type = declarator(type, &name);
        if (is_void(type)) {
            if (nmembers(func)) {
                error("Incomplete type in parameter list.");
            }
            break;
        }
        if (is_array(type)) {
            type = type_create(T_POINTER, type_next(type));
        }
        sym = NULL;
        if (name.len) {
            sym = sym_add(&ns_ident, name, type, SYM_DEFINITION, LINK_NONE);
        }
        type_add_member(func, name, type, sym);
        if (peek().token != ',') {
            break;
        }
        consume(',');
        if (peek().token == DOTS) {
            consume(DOTS);
            assert(!is_vararg(func));
            type_add_member(func, str_init("..."), basic_type__void, NULL);
            assert(is_vararg(func));
            break;
        }
    }

    return func;
}

/*
 * Old-style function definitions with separate identifiers list and
 * type declarations.
 *
 * Return a function type where all members have placeholder type.
 */
static Type identifier_list(Type base)
{
    struct token t;
    Type type;

    type = type_create(T_FUNCTION, base);
    if (peek().token != ')') {
        while (1) {
            t = consume(IDENTIFIER);
            if (get_typedef(t.d.string)) {
                error("Unexpected type '%t' in identifier list.");
                exit(1);
            }
            type_add_member(type, t.d.string, get_type_placeholder(), NULL);
            if (peek().token == ',') {
                next();
            } else break;
        }
    }

    return type;
}

/*
 * Parse array declarations of the form [s0][s1]..[sn], resulting in
 * type [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an
 * incomplete type. Incomplete types are represented by having size of
 * zero.
 */
static Type direct_declarator_array(Type base)
{
    size_t length;
    struct var val;

    if (peek().token == '[') {
        next();
        if (peek().token != ']') {
            val = constant_expression();
            assert(val.kind == IMMEDIATE);
            if (!is_integer(val.type)
                || (is_signed(val.type) && val.imm.i < 1)) {
                error("Array dimension must be a natural number.");
                exit(1);
            }
            length = is_signed(val.type) ? (size_t) val.imm.i : val.imm.u;
        } else {
            length = 0;
        }
        consume(']');
        base = direct_declarator_array(base);
        if (!size_of(base)) {
            error("Array has incomplete element type.");
            exit(1);
        }
        base = type_create(T_ARRAY, base, length);
    }

    return base;
}

/*
 * Parse function and array declarators.
 *
 * Example:
 *
 *    void (*foo)(int)
 *
 * Traverse (*foo) first, and prepended on the outer `(int) -> void`,
 * making it `* (int) -> void`. Void is used as a sentinel, the inner
 * declarator can only produce pointer, function or array.
 */
static Type direct_declarator(Type base, String *name)
{
    struct token t;
    Type type, head = basic_type__void;

    switch (peek().token) {
    case IDENTIFIER:
        t = consume(IDENTIFIER);
        if (!name) {
            error("Unexpected identifier in abstract declarator.");
            exit(1);
        }
        *name = t.d.string;
        break;
    case '(':
        next();
        head = declarator(head, name);
        consume(')');
        break;
    default:
        break;
    }

    switch (peek().token) {
    case '[':
        type = direct_declarator_array(base);
        break;
    case '(':
        next();
        t = peek();
        push_scope(&ns_tag);
        push_scope(&ns_ident);
        if (t.token == IDENTIFIER && !get_typedef(t.d.string)) {
            type = identifier_list(base);
        } else {
            type = parameter_list(base);
        }
        pop_scope(&ns_ident);
        pop_scope(&ns_tag);
        consume(')');
        break;
    default:
        type = base;
        break;
    }

    if (!is_void(head)) {
        type = type_patch_declarator(head, type);
    }

    return type;
}

static Type pointer(Type type)
{
    type = type_create(T_POINTER, type);
    while (1) {
        next();
        switch (peek().token) {
        case CONST:
            type = type_set_const(type);
            break;
        case VOLATILE:
            type = type_set_volatile(type);
            break;
        default:
            return type;
        }
    }
}

Type declarator(Type base, String *name)
{
    while (peek().token == '*') {
        base = pointer(base);
    }

    return direct_declarator(base, name);
}

static void member_declaration_list(Type type)
{
    String name;
    struct var expr;
    Type decl_base, decl_type;

    do {
        decl_base = declaration_specifiers(NULL, NULL);
        do {
            name.len = 0;
            decl_type = declarator(decl_base, &name);
            if (is_struct_or_union(type) && peek().token == ':') {
                if (!is_int(decl_type)) {
                    error("Unsupported type '%t' for bit-field.", decl_type);
                    exit(1);
                }
                consume(':');
                expr = constant_expression();
                if (is_signed(expr.type) && expr.imm.i < 0) {
                    error("Negative width in bit-field.");
                    exit(1);
                }
                type_add_field(type, name, decl_type, expr.imm.i);
            } else {
                if (!name.len) {
                    if (is_struct_or_union(decl_type)) {
                        type_add_anonymous_member(type, decl_type);
                    } else {
                        error("Missing name in member declarator.");
                        exit(1);
                    }
                } else {
                    type_add_member(type, name, decl_type, NULL);
                }
            }
            if (peek().token == ',') {
                consume(',');
                continue;
            }
        } while (peek().token != ';');
        consume(';');
    } while (peek().token != '}');
    type_seal(type);
}

/*
 * Parse and declare a new struct or union type, or retrieve type from
 * existing symbol; possibly providing a complete definition that will
 * be available for later declarations.
 */
static Type struct_or_union_declaration(void)
{
    struct symbol *sym = NULL;
    Type type = {0};
    String name;
    enum type kind;

    kind = (next().token == STRUCT) ? T_STRUCT : T_UNION;
    if (peek().token == IDENTIFIER) {
        name = consume(IDENTIFIER).d.string;
        sym = sym_lookup(&ns_tag, name);
        if (!sym) {
            type = type_create(kind);
            sym = sym_add(&ns_tag, name, type, SYM_TYPEDEF, LINK_NONE);
            type_set_tag(type, sym->name);
        } else if (is_integer(sym->type)) {
            error("Tag '%s' was previously declared as enum.",
                str_raw(sym->name));
            exit(1);
        } else if (type_of(sym->type) != kind) {
            error("Tag '%s' was previously declared as %s.",
                str_raw(sym->name),
                (is_struct(sym->type)) ? "struct" : "union");
            exit(1);
        }
        type = sym->type;
        if (peek().token == '{' && size_of(type)) {
            error("Redefiniton of '%s'.", str_raw(sym->name));
            exit(1);
        }
    }

    if (peek().token == '{') {
        if (!sym) {
            type = type_create(kind);
        }
        next();
        member_declaration_list(type);
        assert(size_of(type));
        consume('}');
    }

    return type;
}

static void enumerator_list(void)
{
    String name;
    struct var val;
    struct symbol *sym;
    int count = 0;

    consume('{');
    do {
        name = consume(IDENTIFIER).d.string;
        if (peek().token == '=') {
            consume('=');
            val = constant_expression();
            if (!is_integer(val.type)) {
                error("Implicit conversion from non-integer type in enum.");
            }
            count = val.imm.i;
        }
        sym = sym_add(
            &ns_ident,
            name,
            basic_type__int,
            SYM_CONSTANT,
            LINK_NONE);
        sym->constant_value.i = count++;
        if (peek().token != ',')
            break;
        consume(',');
    } while (peek().token != '}');
    consume('}');
}

/*
 * Consume enum definition, which represents an int type.
 *
 * Use constant_value as a sentinel to represent definition, checked on
 * lookup to detect duplicate definitions.
 */
static void enum_declaration(void)
{
    String name;
    struct token t;
    struct symbol *tag;

    consume(ENUM);
    t = peek();
    if (t.token == IDENTIFIER) {
        next();
        name = t.d.string;
        tag = sym_lookup(&ns_tag, name);
        if (!tag || tag->depth < current_scope_depth(&ns_tag)) {
            tag = sym_add(
                &ns_tag,
                name,
                basic_type__int,
                SYM_TYPEDEF,
                LINK_NONE);
        } else if (!is_integer(tag->type)) {
            error("Tag '%s' was previously defined as aggregate type.",
                str_raw(tag->name));
            exit(1);
        }
        if (peek().token == '{') {
            if (tag->constant_value.i) {
                error("Redefiniton of enum '%s'.", str_raw(tag->name));
                exit(1);
            }
            enumerator_list();
            tag->constant_value.i = 1;
        }
    } else {
        enumerator_list();
    }
}

/*
 * Parse type, qualifiers and storage class. Do not assume int by
 * default, but require at least one type specifier. Storage class is
 * returned as token value, unless the provided pointer is NULL, in
 * which case the input is parsed as specifier-qualifier-list.
 *
 * Use a compact bit representation to hold state about declaration 
 * specifiers. Initialize storage class to sentinel value.
 */
Type declaration_specifiers(int *storage_class, int *is_inline)
{
    Type type = {-1}, other;
    const Type *tagged;
    struct token tok;

    if (storage_class) {
        *storage_class = '$';
    }

    while (1) {
        switch ((tok = peek()).token) {
        case VOID:
            next();
            type.type = T_VOID;
            break;
        case CHAR:
            next();
            type.type = T_CHAR;
            break;
        case SHORT:
            next();
            type.type = T_SHORT;
            break;
        case INT:
            next();
            if (type.type != T_LONG && type.type != T_SHORT) {
                type.type = T_INT;
            }
            break;
        case SIGNED:
            next();
            if (type.type == -1) {
                type.type = T_INT;
            }
            if (is_unsigned(type)) {
                error("Conflicting 'signed' and 'unsigned' specifiers.");
            }
            break;
        case UNSIGNED:
            next();
            if (type.type == -1) {
                type.type = T_INT;
            }
            if (is_unsigned(type)) {
                error("Duplicate 'unsigned' specifier.");
            }
            type.is_unsigned = 1;
            break;
        case LONG:
            next();
            if (type.type == T_DOUBLE) {
                type.type = T_LDOUBLE;
            } else {
                type.type = T_LONG;
            }
            break;
        case FLOAT:
            next();
            type.type = T_FLOAT;
            break;
        case DOUBLE:
            next();
            if (type.type == T_LONG) {
                type.type = T_LDOUBLE;
            } else {
                type.type = T_DOUBLE;
            }
            break;
        case CONST:
            next();
            type = type_set_const(type);
            break;
        case VOLATILE:
            next();
            type = type_set_volatile(type);
            break;
        case IDENTIFIER:
            tagged = get_typedef(tok.d.string);
            if (tagged) {
                next();
                type = type_apply_qualifiers(*tagged, type);
                break;
            }
            goto done;
        case UNION:
        case STRUCT:
            other = struct_or_union_declaration();
            type = type_apply_qualifiers(other, type);
            break;
        case ENUM:
            enum_declaration();
            type.type = T_INT;
            break;
        case INLINE:
            next();
            if (!is_inline) {
                error("Unexpected 'inline' specifier.");
            } else if (*is_inline) {
                error("Multiple 'inline' specifiers.");
            } else {
                *is_inline = 1;
            }
            break;
        case AUTO:
        case REGISTER:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            next();
            if (!storage_class) {
                error("Unexpected storage class in qualifier list.");
            } else if (*storage_class != '$') {
                error("Multiple storage class specifiers.");
            } else {
                *storage_class = tok.token;
            }
            break;
        default:
            goto done;
        }
    }

done:
    if (type.type == -1) {
        type.type = T_INT;
    }

    return type;
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
    struct block *block,
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
            ? type_create(T_ARRAY, basic_type__char, size)
            : type_create(T_ARRAY, basic_type__long, size / 8);
    case T_ARRAY:
        var = target;
        target.type = type_next(target.type);
        for (i = 0; i < size / size_of(target.type); ++i) {
            target.offset = var.offset + i * size_of(target.type);
            zero_initialize(def, block, target);
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
        eval_assign(def, block, target, as_expr(var));
        break;
    default:
        error("Cannot zero-initialize object of type '%t'.", target.type);
        exit(1);
    }
}

static void zero_initialize_bytes(
    struct definition *def,
    struct block *block,
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
        zero_initialize(def, block, target);
        target.offset += size_of(target.type);
        bytes -= size;
    }
}

static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct var target);

static int is_string(struct expression expr)
{
    return is_identity(expr)
        && expr.l.kind == IMMEDIATE && expr.l.symbol
        && expr.l.symbol->symtype == SYM_STRING_VALUE;
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
        block = initialize_member(def, block, target);
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
            initialize_padding(def, block, target, prev, member);
            target.type = member->type;
            prev = member;
            target.offset = filled + member->offset;
            target.field_offset = member->field_offset;
            target.field_width = member->field_width;
            block = initialize_member(def, block, target);
        } while (i < m && next_element());

        while (i < m) {
            member = get_member(type, i++);
            initialize_padding(def, block, target, prev, member);
            target.type = member->type;
            prev = member;
            target.offset = filled + member->offset;
            target.field_offset = member->field_offset;
            target.field_width = member->field_width;
            zero_initialize(def, block, target);
        }

        initialize_trailing_padding(def, block, target, type);
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
    struct var target)
{
    Type type = target.type;
    size_t filled = target.offset;

    assert(is_array(type));
    assert(target.kind == DIRECT);
    if (is_aggregate(type_next(target.type))) {
        target.type = type_next(target.type);
        do {
            block = initialize_member(def, block, target);
            target.offset += size_of(target.type);
        } while (next_element());
    } else {
        block = read_initializer_element(def, block, target);
        if (is_char(type_next(target.type)) && is_string(block->expr)) {
            target = eval_assign(def, block, target, block->expr);
        } else {
            target.type = type_next(target.type);
            eval_assign(def, block, target, block->expr);
            target.offset += size_of(target.type);
            while (next_element()) {
                block = read_initializer_element(def, block, target);
                eval_assign(def, block, target, block->expr);
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
            zero_initialize(def, block, target);
            target.offset += size_of(target.type);
        }
    }

    return block;
}

/* Initialize member of an aggregate type. */
static struct block *initialize_member(
    struct definition *def,
    struct block *block,
    struct var target)
{
    assert(target.kind == DIRECT);
    if (is_struct_or_union(target.type)) {
        if (peek().token == '{') {
            next();
            block = initialize_struct_or_union(def, block, target);
            if (peek().token == ',')
                next();
            consume('}');
        } else {
            block = initialize_struct_or_union(def, block, target);
        }
    } else if (is_array(target.type)) {
        if (!size_of(target.type)) {
            error("Invalid initialization of flexible array member.");
            exit(1);
        }
        if (peek().token == '{') {
            next();
            block = initialize_array(def, block, target);
            if (peek().token == ',')
                next();
            consume('}');
        } else {
            block = initialize_array(def, block, target);
        }
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, block, target, block->expr);
    }

    return block;
}

/*
 * Parse and emit initializer code for target variable in statements
 * such as int b[] = {0, 1, 2, 3}. Generates a series of assignment
 * operations on references to target variable, with increasing offsets.
 *
 * An initializer can either be an assignment expression, or a brace-
 * enclosed initializer list.
 */
static struct block *initializer(
    struct definition *def,
    struct block *block,
    struct var target)
{
    assert(target.kind == DIRECT);
    if (peek().token == '{') {
        next();
        if (is_struct_or_union(target.type)) {
            block = initialize_struct_or_union(def, block, target);
        } else if (is_array(target.type)) {
            block = initialize_array(def, block, target);
        } else {
            block = initializer(def, block, target);
        }
        if (peek().token == ',')
            next();
        consume('}');
    } else if (is_array(target.type)) {
        block = initialize_array(def, block, target);
    } else {
        block = read_initializer_element(def, block, target);
        eval_assign(def, block, target, block->expr);
    }

    return block;
}

/* Define __func__ as static const char __func__[] = sym->name; */
static void define_builtin__func__(String name)
{
    Type type;
    struct symbol *sym;
    assert(current_scope_depth(&ns_ident) == 1);
    assert(context.standard >= STD_C99);

    /*
     * Just add the symbol directly as a special string value. No
     * explicit assignment reflected in the IR.
     */
    type = type_create(T_ARRAY, basic_type__char, (size_t) name.len + 1);
    sym = sym_add(
        &ns_ident,
        str_init("__func__"),
        type,
        SYM_STRING_VALUE,
        LINK_INTERN);
    sym->string_value = name;
}

/*
 * Parse old-style function definition parameter declarations if present
 * before opening bracket.
 *
 * Verify in the end that all variables have been declared, and add to
 * symbol table parameters that have not been declared old-style.
 * Default to int for parameters that are given without type in the
 * function signature.
 */
static void parameter_declaration_list(struct definition *def, Type type)
{
    int i;
    struct member *param;

    assert(is_function(type));
    assert(current_scope_depth(&ns_ident) == 1);
    while (peek().token != '{') {
        declaration(def, NULL);
    }

    for (i = 0; i < nmembers(type); ++i) {
        param = get_member(type, i);
        if (!param->name.len) {
            error("Missing parameter name at position %d.", i + 1);
            exit(1);
        }
        if (is_type_placeholder(param->type)) {
            param->type = basic_type__int;
        }
        if (!param->sym) {
            param->sym = sym_lookup(&ns_ident, param->name);
            if (!param->sym || param->sym->depth != 1) {
                param->sym = sym_add(&ns_ident,
                    param->name,
                    param->type,
                    SYM_DEFINITION,
                    LINK_NONE);
            }
        } else {
            assert(param->sym->depth == current_scope_depth(&ns_ident));
            sym_make_visible(&ns_ident, param->sym);
        }
        array_push_back(&def->params, param->sym);
    }
}

/*
 * Cover external declarations, functions, and local declarations
 * (with optional initialization code) inside functions.
 */
struct block *declaration(struct definition *def, struct block *parent)
{
    String name;
    Type base, type;
    struct token t;
    struct symbol *sym;
    const struct member *param;
    enum symtype symtype;
    enum linkage linkage;
    int storage_class, is_inline;

    base = declaration_specifiers(&storage_class, &is_inline);
    switch (storage_class) {
    case EXTERN:
        symtype = SYM_DECLARATION;
        linkage = LINK_EXTERN;
        break;
    case STATIC:
        symtype = SYM_TENTATIVE;
        linkage = LINK_INTERN;
        break;
    case TYPEDEF:
        symtype = SYM_TYPEDEF;
        linkage = LINK_NONE;
        break;
    default:
        if (!current_scope_depth(&ns_ident)) {
            symtype = SYM_TENTATIVE;
            linkage = LINK_EXTERN;
        } else {
            symtype = SYM_DEFINITION;
            linkage = LINK_NONE;
        }
        break;
    }

    while (1) {
        name.len = 0;
        type = declarator(base, &name);
        if (!name.len) {
            consume(';');
            return parent;
        }

        if (is_function(type)) {
            symtype = SYM_DECLARATION;
        }

        sym = sym_add(&ns_ident, name, type, symtype, linkage);
        switch (current_scope_depth(&ns_ident)) {
        case 0: break;
        case 1: /* Parameters from old-style function definitions. */
            param = find_type_member(def->symbol->type, name);
            if (param && is_type_placeholder(param->type)) {
                ((struct member *) param)->type = type;
            } else {
                error("Invalid parameter declaration of %s.", str_raw(name));
                exit(1);
            }
            break;
        default:
            array_push_back(&def->locals, sym);
            break;
        }

        switch ((t = peek()).token) {
        case ';':
            consume(';');
            return parent;
        case '=':
            if (sym->symtype == SYM_DECLARATION) {
                error("Extern symbol '%s' cannot be initialized.",
                    str_raw(sym->name));
                exit(1);
            }
            if (!sym->depth && sym->symtype == SYM_DEFINITION) {
                error("Symbol '%s' was already defined.", str_raw(sym->name));
                exit(1);
            }
            consume('=');
            sym->symtype = SYM_DEFINITION;
            if (sym->linkage == LINK_NONE) {
                assert(def);
                assert(parent);
                parent = initializer(def, parent, var_direct(sym));
            } else {
                assert(sym->depth || !parent);
                def = cfg_init(sym);
                initializer(def, def->body, var_direct(sym));
            }
            assert(size_of(sym->type) > 0);
            if (peek().token != ',') {
                consume(';');
                return parent;
            }
            break;
        case IDENTIFIER:
        case FIRST(type_specifier):
        case FIRST(type_qualifier):
        case REGISTER:
        case '{':
            assert(!parent);
            assert(sym->linkage != LINK_NONE);
            if (is_function(sym->type)) {
                sym->symtype = SYM_DEFINITION;
                def = cfg_init(sym);
                push_scope(&ns_label);
                push_scope(&ns_ident);
                parameter_declaration_list(def, type);
                if (context.standard >= STD_C99) {
                    define_builtin__func__(sym->name);
                }
                parent = block(def, def->body);
                pop_scope(&ns_label);
                pop_scope(&ns_ident);
                return parent;
            }
        default: break;
        }
        consume(',');
    }
}
