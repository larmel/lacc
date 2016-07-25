#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "statement.h"
#include "symtab.h"
#include "type.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *initializer(
    struct definition *def,
    struct block *block,
    struct var target);

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list; even
 * though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static struct typetree *parameter_list(const struct typetree *base)
{
    struct typetree *func = type_init(T_FUNCTION);
    func->next = base;

    while (peek().token != ')') {
        String name;
        struct typetree *type;

        name.len = 0;
        type = declaration_specifiers(NULL);
        type = declarator(type, &name);
        if (is_void(type)) {
            if (nmembers(func)) {
                error("Incomplete type in parameter list.");
            }
            break;
        }

        type_add_member(func, name, type);
        if (peek().token != ',') {
            break;
        }

        consume(',');
        if (peek().token == ')') {
            error("Unexpected trailing comma in parameter list.");
            exit(1);
        } else if (peek().token == DOTS) {
            consume(DOTS);
            assert(!is_vararg(func));
            type_add_member(func, str_init("..."), NULL);
            assert(is_vararg(func));
            break;
        }
    }

    return func;
}

/* Parse array declarations of the form [s0][s1]..[sn], resulting in
 * type [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an
 * incomplete type. Incomplete types are represented by having size of
 * zero.
 */
static struct typetree *direct_declarator_array(struct typetree *base)
{
    if (peek().token == '[') {
        long length = 0;

        consume('[');
        if (peek().token != ']') {
            struct var expr = constant_expression();
            assert(expr.kind == IMMEDIATE);
            if (!is_integer(expr.type) || expr.imm.i < 1) {
                error("Array dimension must be a natural number.");
                exit(1);
            }
            length = expr.imm.i;
        }
        consume(']');

        base = direct_declarator_array(base);
        if (!size_of(base)) {
            error("Array has incomplete element type.");
            exit(1);
        }

        base = type_init(T_ARRAY, base, length);
    }

    return base;
}

/* Parse function and array declarators. Some trickery is needed to
 * handle declarations like `void (*foo)(int)`, where the inner *foo
 * has to be  traversed first, and prepended on the outer type
 * `* (int) -> void` afterwards making it `* (int) -> void`.
 * The type returned from declarator has to be either array, function or
 * pointer, thus only need to check for type->next to find inner tail.
 */
static struct typetree *direct_declarator(
    struct typetree *base,
    String *name)
{
    struct typetree *type = base;
    struct typetree *head = NULL, *tail = NULL;
    struct token ident;

    switch (peek().token) {
    case IDENTIFIER:
        ident = consume(IDENTIFIER);
        if (!name) {
            error("Unexpected identifier in abstract declarator.");
            exit(1);
        }
        *name = ident.d.string;
        break;
    case '(':
        consume('(');
        tail = declarator(NULL, name);
        if (tail) {
            head = tail;
            type = tail;
            while (tail->next) {
                tail = (struct typetree *) tail->next;
            }
        }
        consume(')');
        break;
    default:
        break;
    }

    while (peek().token == '[' || peek().token == '(') {
        switch (peek().token) {
        case '[':
            type = direct_declarator_array(base);
            break;
        case '(':
            consume('(');
            type = parameter_list(base);
            consume(')');
            break;
        default:
            assert(0);
        }
        if (tail) {
            assert(head);
            tail->next = type;
            type = head;
        }
        base = type;
    }

    return type;
}

static struct typetree *pointer(const struct typetree *base)
{
    String s;
    struct typetree *type = type_init(T_POINTER, base);

    #define set_qualifier(arg) \
        if (type->qualifier & arg) \
            error("Duplicate type qualifier '%s'.", str_raw(s)); \
        type->qualifier |= arg;

    consume('*');
    while (1) {
        s = peek().d.string;
        if (peek().token == CONST) {
            set_qualifier(Q_CONST);
        } else if (peek().token == VOLATILE) {
            set_qualifier(Q_VOLATILE);
        } else break;
        next();
    }

    #undef set_qualifier

    return type;
}

struct typetree *declarator(struct typetree *base, String *name)
{
    while (peek().token == '*') {
        base = pointer(base);
    }

    return direct_declarator(base, name);
}

static void member_declaration_list(struct typetree *type)
{
    struct namespace ns = {0};
    String name;
    struct var expr;
    struct typetree *decl_base, *decl_type;

    push_scope(&ns);
    do {
        decl_base = declaration_specifiers(NULL);
        do {
            name.len = 0;
            decl_type = declarator(decl_base, &name);
            if (is_struct_or_union(type) && peek().token == ':') {
                if (!is_integer(decl_type) || decl_type->size != 4) {
                    error("Unsupported type '%t' for bit-field.", decl_type);
                    exit(1);
                }
                consume(':');
                expr = constant_expression();
                if (is_signed(expr.type) && expr.imm.i < 0) {
                    error("Negative width in bit-field.");
                    exit(1);
                }
                if (name.len) {
                    sym_add(&ns, name, decl_type, SYM_DECLARATION, LINK_NONE);
                }
                type_add_field(type, name, decl_type, expr.imm.i);
            } else {
                if (!name.len) {
                    error("Missing name in member declarator.");
                    exit(1);
                } else if (!size_of(decl_type)) {
                    error("Member '%s' has incomplete type '%t'.",
                        name.len, decl_type);
                    exit(1);
                }
                sym_add(&ns, name, decl_type, SYM_DECLARATION, LINK_NONE);
                type_add_member(type, name, decl_type);
            }

            if (peek().token == ',') {
                consume(',');
                continue;
            }
        } while (peek().token != ';');
        consume(';');
    } while (peek().token != '}');
    pop_scope(&ns);
}

static struct typetree *struct_or_union_declaration(void)
{
    struct symbol *sym = NULL;
    struct typetree *type = NULL;
    String name;
    enum type kind =
        (next().token == STRUCT) ? T_STRUCT : T_UNION;

    if (peek().token == IDENTIFIER) {
        name = consume(IDENTIFIER).d.string;
        sym = sym_lookup(&ns_tag, name);
        if (!sym) {
            type = type_init(kind);
            sym = sym_add(&ns_tag, name, type, SYM_TYPEDEF, LINK_NONE);
        } else if (is_integer(&sym->type)) {
            error("Tag '%s' was previously declared as enum.", str_raw(sym->name));
            exit(1);
        } else if (sym->type.type != kind) {
            error("Tag '%s' was previously declared as %s.",
                str_raw(sym->name), (is_struct(&sym->type)) ? "struct" : "union");
            exit(1);
        }

        /* Retrieve type from existing symbol, possibly providing a
         * complete definition that will be available for later
         * declarations. Overwrites existing type information from
         * symbol table. */
        type = &sym->type;
        if (peek().token == '{' && type->size) {
            error("Redefiniton of '%s'.", str_raw(sym->name));
            exit(1);
        }
    }

    if (peek().token == '{') {
        if (!type) {
            /* Anonymous structure; allocate a new standalone type,
             * not part of any symbol. */
            type = type_init(kind);
        }

        consume('{');
        member_declaration_list(type);
        assert(type->size);
        consume('}');
    }

    /* Return to the caller a copy of the root node, which can be
     * overwritten with new type qualifiers without altering the tag
     * registration. */
    return (sym) ? type_tagged_copy(&sym->type, sym->name) : type;
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
            &basic_type__int,
            SYM_CONSTANT,
            LINK_NONE);

        sym->constant_value.i = count++;
        if (peek().token != ',')
            break;

        consume(',');
    } while (peek().token != '}');
    consume('}');
}

static struct typetree *enum_declaration(void)
{
    String name;
    struct symbol *tag;
    struct typetree *type = type_init(T_SIGNED, 4);

    consume(ENUM);
    if (peek().token == IDENTIFIER) {
        name = consume(IDENTIFIER).d.string;
        tag = sym_lookup(&ns_tag, name);
        if (!tag || tag->depth < current_scope_depth(&ns_tag)) {
            tag = sym_add(&ns_tag, name, type, SYM_TYPEDEF, LINK_NONE);
        } else if (!is_integer(&tag->type)) {
            error("Tag '%s' was previously defined as aggregate type.",
                str_raw(tag->name));
            exit(1);
        }

        /* Use constant_value as a sentinel to represent definition,
         * checked on  lookup to detect duplicate definitions. */
        if (peek().token == '{') {
            if (tag->constant_value.i) {
                error("Redefiniton of enum '%s'.", str_raw(tag->name));
            }
            enumerator_list();
            tag->constant_value.i = 1;
        }
    } else {
        enumerator_list();
    }

    /* Result is always integer. Do not care about the actual enum
     * definition, all enums are ints and no type checking is done. */
    return type;
}

static struct typetree get_basic_type_from_specifier(unsigned short spec)
{
    switch (spec) {
    case 0x0001: /* void */
        return basic_type__void;
    case 0x0002: /* char */
    case 0x0012: /* signed char */
        return basic_type__char;
    case 0x0022: /* unsigned char */
        return basic_type__unsigned_char;
    case 0x0004: /* short */
    case 0x0014: /* signed short */
    case 0x000C: /* short int */
    case 0x001C: /* signed short int */
        return basic_type__short;
    case 0x0024: /* unsigned short */
    case 0x002C: /* unsigned short int */
        return basic_type__unsigned_short;
    case 0x0008: /* int */
    case 0x0010: /* signed */
    case 0x0018: /* signed int */
        return basic_type__int;
    case 0x0020: /* unsigned */
    case 0x0028: /* unsigned int */
        return basic_type__unsigned_int;
    case 0x0040: /* long */
    case 0x0048: /* long int */
    case 0x00C0: /* long long */
    case 0x00C8: /* long long int */
    case 0x0050: /* signed long */
    case 0x0058: /* signed long int */
    case 0x00D0: /* signed long long */
    case 0x00D8: /* signed long long int */
        return basic_type__long;
    case 0x0060: /* unsigned long */
    case 0x0068: /* unsigned long int */
    case 0x00E0: /* unsigned long long */
    case 0x00E8: /* unsigned long long int */
        return basic_type__unsigned_long;
    case 0x0100: /* float */
        return basic_type__float;
    case 0x0200: /* double */
    case 0x0240: /* long double */
        return basic_type__double;
    default:
        error("Invalid type specification.");
        exit(1); 
    }
}

/* Parse type, qualifiers and storage class. Do not assume int by
 * default, but require at least one type specifier. Storage class is
 * returned as token value, unless the provided pointer is NULL, in
 * which case the input is parsed as specifier-qualifier-list.
 */
struct typetree *declaration_specifiers(int *stc)
{
    struct typetree *type = NULL;
    struct token tok;
    int done = 0;

    /* Use a compact bit representation to hold state about declaration 
     * specifiers. Initialize storage class to sentinel value. */
    unsigned short spec = 0x0000;
    enum qualifier qual = Q_NONE;
    if (stc)       *stc =    '$';

    #define set_specifier(arg) \
        if (spec & arg)                                                        \
            error("Duplicate type specifier '%s'.", str_raw(tok.d.string));    \
        next(); spec |= arg;

    #define set_qualifier(arg) \
        if (qual & arg)                                                        \
            error("Duplicate type qualifier '%s'.", str_raw(tok.d.string));    \
        next(); qual |= arg;

    #define set_storage_class(t) \
        if (!stc) error("Unexpected storage class in qualifier list.");        \
        else if (*stc != '$') error("Multiple storage class specifiers.");     \
        next(); *stc = t;

    do {
        switch ((tok = peek()).token) {
        case VOID:      set_specifier(0x001); break;
        case CHAR:      set_specifier(0x002); break;
        case SHORT:     set_specifier(0x004); break;
        case INT:       set_specifier(0x008); break;
        case SIGNED:    set_specifier(0x010); break;
        case UNSIGNED:  set_specifier(0x020); break;
        case LONG:
            if (spec & 0x040) {
                set_specifier(0x080);
            } else {
                set_specifier(0x040);   
            }
            break;
        case FLOAT:     set_specifier(0x100); break;
        case DOUBLE:    set_specifier(0x200); break;
        case CONST:     set_qualifier(Q_CONST); break;
        case VOLATILE:  set_qualifier(Q_VOLATILE); break;
        case IDENTIFIER: {
            struct symbol *tag = sym_lookup(&ns_ident, tok.d.string);
            if (tag && tag->symtype == SYM_TYPEDEF && !type) {
                consume(IDENTIFIER);
                type = type_init(T_STRUCT);
                *type = tag->type;
            } else {
                done = 1;
            }
            break;
        }
        case UNION:
        case STRUCT:
            if (!type) {
                type = struct_or_union_declaration();
            } else {
                done = 1;
            }
            break;
        case ENUM:
            if (!type) {
                type = enum_declaration();
            } else {
                done = 1;
            }
            break;
        case AUTO:
        case REGISTER:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            set_storage_class(tok.token);
            break;
        default:
            done = 1;
            break;
        }

        if (type && spec) {
            error("Invalid combination of declaration specifiers.");
            exit(1);
        }
    } while (!done);

    #undef set_specifier
    #undef set_qualifier
    #undef set_storage_class

    if (type) {
        if (qual & type->qualifier) {
            error("Duplicate type qualifier:%s%s.",
                (qual & Q_CONST) ? " const" : "",
                (qual & Q_VOLATILE) ? " volatile" : "");
        }
    } else if (spec) {
        type = type_init(T_STRUCT);
        *type = get_basic_type_from_specifier(spec);
    } else {
        error("Missing type specifier.");
        exit(1);
    }

    type->qualifier |= qual;
    return type;
}

/*
 * Constants representing immediate zero values of all basic types.
 */
static const struct var
    var__zero_float = {&basic_type__float, NULL, IMMEDIATE},
    var__zero_double = {&basic_type__double, NULL, IMMEDIATE},
    var__zero_unsigned[] = {
        {&basic_type__unsigned_char, NULL, IMMEDIATE},
        {&basic_type__unsigned_short, NULL, IMMEDIATE},
        {0},
        {&basic_type__unsigned_int, NULL, IMMEDIATE},
        {0},
        {0},
        {0},
        {&basic_type__unsigned_long, NULL, IMMEDIATE}
    },
    var__zero_signed[] = {
        {&basic_type__char, NULL, IMMEDIATE},
        {&basic_type__short, NULL, IMMEDIATE},
        {0},
        {&basic_type__int, NULL, IMMEDIATE},
        {0},
        {0},
        {0},
        {&basic_type__long, NULL, IMMEDIATE}
    };

/*
 * Set var = 0, using simple assignment on members for composite types.
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
    struct var var;
    const struct member *member;
    assert(target.kind == DIRECT);

    switch (target.type->type) {
    case T_STRUCT:
        target.type = unwrapped(target.type);
        var = target;
        for (i = 0; i < nmembers(var.type); ++i) {
            member = get_member(var.type, i);
            target.type = member->type;
            target.offset = var.offset + member->offset;
            zero_initialize(def, block, target);
        }
        break;
    case T_UNION:
        /* We don't want garbage in any union member after zero-
           initialization, so set full width to zero. */
        target.type =
            (size_of(target.type) % 8) ?
                type_init(T_ARRAY, &basic_type__char, size_of(target.type)) :
                type_init(T_ARRAY, &basic_type__long, size_of(target.type) / 8);
        zero_initialize(def, block, target);
        break;
    case T_ARRAY:
        assert(target.type->size);
        var = target;
        target.type = target.type->next;
        assert(is_struct(target.type) || !target.type->next);
        for (i = 0; i < var.type->size / var.type->next->size; ++i) {
            target.offset = var.offset + i * var.type->next->size;
            zero_initialize(def, block, target);
        }
        break;
    case T_POINTER:
        var = var__zero_unsigned[7];
        var.type = target.type;
        eval_assign(def, block, target, var);
        break;
    case T_UNSIGNED:
        var = var__zero_unsigned[size_of(target.type) - 1];
        eval_assign(def, block, target, var);
        break;
    case T_SIGNED:
        var = var__zero_signed[size_of(target.type) - 1];
        eval_assign(def, block, target, var);
        break;
    case T_REAL:
        var = (is_float(target.type) ? var__zero_float : var__zero_double);
        eval_assign(def, block, target, var);
        break;
    default:
        error("Cannot zero-initialize object of type '%t'.", target.type);
        exit(1);
    }
}

static struct block *object_initializer(
    struct definition *def,
    struct block *block,
    struct var target)
{
    int i, filled = target.offset;
    const struct typetree *type = target.type;
    const struct member *member;

    assert(!is_tagged(type));

    consume('{');
    target.lvalue = 1;
    switch (type->type) {
    case T_UNION:
        member = get_member(type, 0);
        target.type = member->type;
        block = initializer(def, block, target);
        if (size_of(member->type) < type->size) {
            /* Only the first element of a union can be initialized.
             * Zero the remaining memory if there is padding, or the
             * first member is not the largest one. */
            target.type =
                type_init(
                    T_ARRAY,
                    &basic_type__char,
                    type->size - size_of(member->type));
            target.offset += size_of(member->type);
            zero_initialize(def, block, target);
        }
        if (peek().token != '}') {
            error("Excess elements in union initializer.");
            exit(1);
        }
        break;
    case T_STRUCT:
        for (i = 0; i < nmembers(type); ++i) {
            member = get_member(type, i);
            target.type = member->type;
            target.offset = filled + member->offset;
            block = initializer(def, block, target);
            if (peek().token == ',') {
                consume(',');
            } else break;
            if (peek().token == '}') {
                break;
            }
        }
        while (++i < nmembers(type)) {
            member = get_member(type, i);
            target.type = member->type;
            target.offset = filled + member->offset;
            zero_initialize(def, block, target);
        }
        break;
    case T_ARRAY:
        target.type = type->next;
        for (i = 0; !type->size || i < type->size / size_of(type->next); ++i) {
            target.offset = filled + i * size_of(type->next);
            block = initializer(def, block, target);
            if (peek().token == ',') {
                consume(',');
            } else break;
            if (peek().token == '}') {
                break;
            }
        }
        if (!type->size) {
            assert(!target.symbol->type.size);
            assert(is_array(&target.symbol->type));

            /* Incomplete array type can only be in the root level of
             * target type tree, overwrite type directly in symbol. */
            ((struct symbol *) target.symbol)->type.size =
                (i + 1) * size_of(type->next);
        } else {
            while (++i < type->size / size_of(type->next)) {
                target.offset = filled + i * size_of(type->next);
                zero_initialize(def, block, target);
            }
        }
        break;
    default:
        error("Block initializer only apply to aggregate or union type.");
        exit(1);
    }

    consume('}');
    return block;
}

static int is_string(struct var val)
{
    return
        val.kind == IMMEDIATE && val.symbol &&
        val.symbol->symtype == SYM_STRING_VALUE;
}

/* Assignment between array and string literal. Handle special case of
 * incomplete array type, and assignment to arrays which are longer than
 * the string itself. In that case, the rest of the array is initialized
 * to zero.
 *
 *      int foo[4] = "Hi"
 *
 * This will generates the following IR assignments:
 *
 *      foo = "Hi"
 *      foo[3] = 0
 *      foo[4] = 0
 */
static struct block *string_initializer(
    struct definition *def,
    struct block *block,
    struct var target)
{
    const struct typetree *type;
    assert(target.kind == DIRECT);
    assert(is_array(target.type));
    assert(is_string(block->expr));

    if (type_equal(target.type->next, block->expr.type->next) &&
        size_of(target.type) > size_of(block->expr.type))
    {
        type = target.type;
        target.type = type_init(
            T_ARRAY,
            target.type->next,
            size_of(block->expr.type) / size_of(type->next));
        eval_assign(def, block, target, block->expr);

        assert(size_of(type) > size_of(target.type));
        target.offset += size_of(target.type);
        target.type = type_init(
            T_ARRAY,
            target.type->next,
            (size_of(type) - size_of(target.type)) / size_of(type->next));
        zero_initialize(def, block, target);
    } else {
        if (!target.type->size) {
            assert(!target.offset);

            /* Complete type based on string literal. Evaluation does
             * not have the required context to do this logic. */
            ((struct symbol *) target.symbol)->type.size =
                block->expr.type->size;
            target.type = block->expr.type;
        }

        eval_assign(def, block, target, block->expr);
    }

    return block;
}

/*
 * Parse and emit initializer code for target variable in statements
 * such as int b[] = {0, 1, 2, 3}. Generates a series of assignment
 * operations on references to target variable, with increasing offsets.
 */
static struct block *initializer(
    struct definition *def,
    struct block *block,
    struct var target)
{
    int ops;
    assert(target.kind == DIRECT);

    /* Do not care about cv-qualifiers here. */
    target.type = unwrapped(target.type);

    if (peek().token == '{') {
        block = object_initializer(def, block, target);
    } else {
        ops = array_len(&block->code);
        block = assignment_expression(def, block);
        if (target.symbol->linkage != LINK_NONE
            && (array_len(&block->code) - ops > 0
                || (block->expr.kind != IMMEDIATE
                    && block->expr.kind != ADDRESS)
                || (block->expr.kind == ADDRESS
                    && block->expr.symbol->linkage == LINK_NONE)))
        {
            error("Initializer must be computable at load time.");
            exit(1);
        }

        if (is_array(target.type) && is_string(block->expr)) {
            block = string_initializer(def, block, target);
        } else {
            /* Make sure basic types are converted, but avoid invalid
               cast for struct or union types. */
            if (!type_equal(target.type, block->expr.type)) {
                block->expr = eval_cast(def, block, block->expr, target.type);
            }

            eval_assign(def, block, target, block->expr);
        }
    }

    return block;
}

/* Define __func__ as static const char __func__[] = sym->name;
 */
static void define_builtin__func__(String name)
{
    struct typetree *type;
    struct symbol *sym;
    assert(current_scope_depth(&ns_ident) == 1);
    assert(context.standard == STD_C99);

    /* Just add the symbol directly as a special string value. No
     * explicit assignment reflected in the IR. */
    type = type_init(T_ARRAY, &basic_type__char, name.len + 1);
    sym = sym_add(
        &ns_ident,
        str_init("__func__"),
        type,
        SYM_STRING_VALUE,
        LINK_INTERN);
    sym->string_value = name;
}

/* Cover both external declarations, functions, and local declarations
 * (with optional initialization code) inside functions.
 */
struct block *declaration(struct definition *def, struct block *parent)
{
    struct typetree *base;
    enum symtype symtype;
    enum linkage linkage;
    int stc = '$';

    base = declaration_specifiers(&stc);
    switch (stc) {
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
        String name;
        const struct typetree *type;
        struct symbol *sym;

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
        if (current_scope_depth(&ns_ident)) {
            assert(current_scope_depth(&ns_ident) > 1);
            assert(def);
            array_push_back(&def->locals, sym);
        }

        switch (peek().token) {
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
            assert(size_of(&sym->type) > 0);
            if (peek().token != ',') {
                consume(';');
                return parent;
            }
            break;
        case '{': {
            int i;
            if (!is_function(&sym->type) || sym->depth) {
                error("Invalid function definition.");
                exit(1);
            }
            assert(!parent);
            assert(sym->linkage != LINK_NONE);
            sym->symtype = SYM_DEFINITION;
            def = cfg_init(sym);
            push_scope(&ns_ident);
            push_scope(&ns_label);
            if (context.standard >= STD_C99) {
                define_builtin__func__(sym->name);
            }
            for (i = 0; i < nmembers(&sym->type); ++i) {
                name = get_member(&sym->type, i)->name;
                type = get_member(&sym->type, i)->type;
                symtype = SYM_DEFINITION;
                linkage = LINK_NONE;
                if (!name.len) {
                    error("Missing parameter name at position %d.", i + 1);
                    exit(1);
                }
                array_push_back(&def->params,
                    sym_add(&ns_ident, name, type, symtype, linkage));
            }
            parent = block(def, def->body);
            pop_scope(&ns_label);
            pop_scope(&ns_ident);
            return parent;
        }
        default:
            break;
        }
        consume(',');
    }
}
