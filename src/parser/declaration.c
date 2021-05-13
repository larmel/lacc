#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "statement.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static const Type *get_typedef(String str)
{
    struct symbol *tag;

    tag = sym_lookup(&ns_ident, str);
    if (tag && tag->symtype == SYM_TYPEDEF) {
        return &tag->type;
    }

    return NULL;
}

static struct block *parameter_declarator(
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length);

/*
 * Parse a function parameter list, adding symbols to scope.
 *
 * FOLLOW(parameter-list) = { ')' }, peek to return empty list; even
 * though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 *
 * As a special case, ignore evaluation when in block scope. This is to
 * avoid VLA code that would be generated in cases like this:
 *
 *     int main(void) {
 *         int foo(int n, int arr[][n + 1]);
 *         return 0;
 *     }
 *
 * The evaluation of n + 1 is done in a throwaway block, and not
 * included in the CFG of main.
 */
static struct block *parameter_list(
    struct definition *def,
    struct block *parent,
    Type base,
    Type *func)
{
    static String dots = SHORT_STRING_INIT("...");

    String name;
    size_t length;
    struct block *block;
    struct member *param;
    struct declaration_specifier_info info;

    *func = type_create_function(base);
    block = current_scope_depth(&ns_ident) == 1
        ? parent
        : cfg_block_init(def);

    while (peek() != ')') {
        name = str_empty();
        length = 0;
        base = declaration_specifiers(&info);
        if (info.storage_class) {
            error("Unexpected storage class in parameter list.");
        } else if (info.is_inline) {
            error("Parameter cannot be declared inline.");
        }

        block = parameter_declarator(def, block, base, &base, &name, &length);
        if (is_void(base)) {
            if (nmembers(*func)) {
                error("Incomplete type in parameter list.");
                exit(1);
            }
            type_seal(*func);
            break;
        } else if (is_array(base)) {
            base = type_create_pointer(type_next(base));
        } else if (is_function(base)) {
            base = type_create_pointer(base);
        }

        param = type_add_member(*func, name, base);
        param->offset = length;
        if (!str_is_empty(name)) {
            param->sym =
                sym_add(&ns_ident, name, base, SYM_DEFINITION, LINK_NONE);
        }

        if (!try_consume(',')) {
            break;
        }

        if (try_consume(DOTS)) {
            assert(!is_vararg(*func));
            type_add_member(*func, dots, basic_type__void);
            assert(is_vararg(*func));
            break;
        }
    }

    return current_scope_depth(&ns_ident) == 1 ? block : parent;
}

/*
 * Old-style function definitions with separate identifiers list and
 * type declarations.
 *
 * Return a function type where all members have placeholder type.
 */
static Type identifier_list(Type base)
{
    String str;
    Type type;

    type = type_create_function(base);
    if (peek() != ')') {
        while (1) {
            consume(IDENTIFIER);
            str = access_token(0)->d.string;
            if (get_typedef(str)) {
                error("Unexpected type '%t' in identifier list.");
                exit(1);
            }

            type_add_member(type, str, get_type_placeholder());
            if (peek() == ',') {
                next();
            } else break;
        }
    }

    return type;
}

struct array_param {
    char is_const;
    char is_volatile;
    char is_restrict;
    char is_static;
};

static void array_param_qualifiers(struct array_param *cvrs)
{
    cvrs->is_static = try_consume(STATIC);
    while (1) {
        switch (peek()) {
        case CONST:
            cvrs->is_const = 1;
            next();
            continue;
        case VOLATILE:
            cvrs->is_volatile = 1;
            next();
            continue;
        case RESTRICT:
            cvrs->is_restrict = 1;
            next();
            continue;
        default:
            break;
        }
        break;
    }

    cvrs->is_static = cvrs->is_static || try_consume(STATIC);
}

/*
 * Parse array declarations of the form [s0][s1]..[sn], resulting in
 * type [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an
 * incomplete type. Incomplete types are represented by having size of
 * zero.
 *
 * VLA require evaluating an expression, and storing it in a separate
 * stack allocated variable.
 */
static struct block *array_declarator(
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    size_t *static_length)
{
    size_t length = 0;
    struct var val;
    struct array_param cvrs = {0};
    int is_incomplete = 0;
    const struct symbol *sym = NULL;

    consume('[');
    if (peek() == ']') {
        is_incomplete = 1;
    } else {
        if (!def) {
            val = constant_expression();
            block = cfg_block_init(NULL);
        } else {
            assert(block);
            if (static_length) {
                array_param_qualifiers(&cvrs);
                if (cvrs.is_static && peek() == ']') {
                    error("Missing array length.");
                    exit(1);
                }
            }

            if (peek() != ']') {
                block = assignment_expression(def, block);
                val = eval(def, block, block->expr);
            } else {
                is_incomplete = 1;
            }
        }
    }

    if (!is_incomplete) {
        if (!is_integer(val.type)) {
            error("Array dimension must be of integer type.");
            exit(1);
        }
        if (val.kind == IMMEDIATE && is_signed(val.type) && val.imm.i < 0) {
            error("Array dimension must be a positive number.");
            exit(1);
        }

        if (!type_equal(val.type, basic_type__unsigned_long)) {
            val = eval(def, block,
                eval_cast(def, block, val, basic_type__unsigned_long));
        } else if (val.kind == DIRECT && !is_temporary(val.symbol)) {
            val = eval_copy(def, block, val);
        }

        assert(is_unsigned(val.type));
        assert(type_equal(val.type, basic_type__unsigned_long));

        block->expr = as_expr(val);
        if (val.kind == IMMEDIATE) {
            length = val.imm.u;
        } else {
            assert(val.kind == DIRECT);
            assert(val.symbol);
            sym = val.symbol;
        }
    }

    consume(']');
    if (peek() == '[') {
        block = array_declarator(def, block, base, &base, NULL);
    }

    if (!is_complete(base)) {
        error("Array has incomplete element type.");
        exit(1);
    }

    if (static_length) {
        *static_length = length;
        *type = type_create_pointer(base);
        if (cvrs.is_const) *type = type_set_const(*type);
        if (cvrs.is_volatile) *type =  type_set_volatile(*type);
        if (cvrs.is_restrict) *type = type_set_restrict(*type);
    } else if (is_incomplete) {
        *type = type_create_incomplete(base);
    } else if (sym) {
        *type = type_create_vla(base, sym);
    } else {
        *type = type_create_array(base, length);
    }

    return block;
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
static struct block *direct_declarator(
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length)
{
    enum token_type t;
    Type head = basic_type__void;

    switch (peek()) {
    case IDENTIFIER:
        next();
        if (!name) {
            error("Unexpected identifier in abstract declarator.");
            exit(1);
        }
        *name = access_token(0)->d.string;
        break;
    case '(':
        t = peekn(2);
        if ((t == IDENTIFIER && !get_typedef(access_token(2)->d.string))
            || t == '('
            || t == '*')
        {
            next();
            block = declarator(def, block, head, &head, name);
            consume(')');
            if (!is_void(head)) {
                length = NULL;
            }
        }
        break;
    default:
        break;
    }

    switch (peek()) {
    case '[':
        block = array_declarator(def, block, base, type, length);
        break;
    case '(':
        next();
        t = peek();
        push_scope(&ns_tag);
        push_scope(&ns_ident);
        if (t == IDENTIFIER && !get_typedef(access_token(1)->d.string)) {
            *type = identifier_list(base);
        } else {
            block = parameter_list(def, block, base, type);
        }
        pop_scope(&ns_ident);
        pop_scope(&ns_tag);
        consume(')');
        break;
    default:
        *type = base;
        break;
    }

    if (!is_void(head)) {
        *type = type_patch_declarator(head, *type);
    }

    return block;
}

static Type pointer(Type type)
{
    type = type_create_pointer(type);
    while (1) {
        next();
        switch (peek()) {
        case CONST:
            type = type_set_const(type);
            break;
        case VOLATILE:
            type = type_set_volatile(type);
            break;
        case RESTRICT:
            type = type_set_restrict(type);
            break;
        default:
            return type;
        }
    }
}

static struct block *parameter_declarator(
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length)
{
    assert(type);
    while (peek() == '*') {
        base = pointer(base);
    }

    return direct_declarator(def, block, base, type, name, length);
}

INTERNAL struct block *declarator(
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name)
{
    return parameter_declarator(def, block, base, type, name, NULL);
}

static void member_declaration_list(Type type)
{
    String name;
    struct var expr;
    Type decl_base, decl_type;

    do {
        decl_base = declaration_specifiers(NULL);
        while (1) {
            name = str_empty();
            declarator(NULL, NULL, decl_base, &decl_type, &name);
            if (is_struct_or_union(type) && peek() == ':') {
                if (!is_integer(decl_type)) {
                    error("Unsupported type '%t' for bit-field.", decl_type);
                    exit(1);
                }

                consume(':');
                expr = constant_expression();
                if (is_signed(expr.type) && expr.imm.i < 0) {
                    error("Negative width in bit-field.");
                    exit(1);
                }

                type_add_field(type, name, decl_type, expr.imm.u);
            } else if (str_is_empty(name)) {
                if (is_struct_or_union(decl_type)) {
                    type_add_anonymous_member(type, decl_type);
                } else {
                    error("Missing name in member declarator.");
                    exit(1);
                }
            } else {
                type_add_member(type, name, decl_type);
            }

            if (!try_consume(',')) {
                break;
            }
        }

        consume(';');
    } while (peek() != '}');

    type_seal(type);
}

/*
 * Parse and declare a new struct or union type, or retrieve type from
 * existing symbol; possibly providing a complete definition that will
 * be available for later declarations.
 */
static Type struct_or_union_declaration(enum token_type t)
{
    struct symbol *sym = NULL;
    Type type = {0};
    String name;
    enum type kind;

    assert(t == STRUCT || t == UNION);
    kind = (t == STRUCT) ? T_STRUCT : T_UNION;
    if (try_consume(IDENTIFIER)) {
        name = access_token(0)->d.string;
        sym = sym_lookup(&ns_tag, name);
        if (!sym) {
            type = type_create(kind);
            sym = sym_add(&ns_tag, name, type, SYM_TAG, LINK_NONE);
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
        if (peek() == '{' && size_of(type)) {
            error("Redefiniton of '%s'.", str_raw(sym->name));
            exit(1);
        }
    }

    if (try_consume('{')) {
        if (!sym) {
            type = type_create(kind);
        }

        member_declaration_list(type);
        assert(size_of(type));
        consume('}');
    } else if (!sym) {
        error("Invalid declaration.");
        exit(1);
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
        consume(IDENTIFIER);
        name = access_token(0)->d.string;
        if (try_consume('=')) {
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
        sym->value.constant.i = count++;
        if (!try_consume(','))
            break;
    } while (peek() != '}');
    consume('}');
}

/*
 * Consume enum definition, which represents an int type.
 *
 * Use value.constant as a sentinel to represent definition, checked on
 * lookup to detect duplicate definitions.
 */
static void enum_declaration(void)
{
    String name;
    struct symbol *tag;

    if (try_consume(IDENTIFIER)) {
        name = access_token(0)->d.string;
        tag = sym_lookup(&ns_tag, name);
        if (!tag || tag->depth < current_scope_depth(&ns_tag)) {
            tag = sym_add(
                &ns_tag,
                name,
                basic_type__int,
                SYM_TAG,
                LINK_NONE);
        } else if (!is_integer(tag->type)) {
            error("Tag '%s' was previously defined as aggregate type.",
                str_raw(tag->name));
            exit(1);
        }
        if (peek() == '{') {
            if (tag->value.constant.i) {
                error("Redefiniton of enum '%s'.", str_raw(tag->name));
                exit(1);
            }
            enumerator_list();
            tag->value.constant.i = 1;
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
 * Type specifiers must be one of the following permutations:
 *
 *     void
 *     char
 *     signed char
 *     unsigned char
 *     short, signed short, short int, or signed short int
 *     unsigned short, or unsigned short int
 *     int, signed, or signed int
 *     unsigned, or unsigned int
 *     long, signed long, long int, or signed long int
 *     unsigned long, or unsigned long int
 *     long long, signed long long, long long int, or
 *     signed long long int
 *     unsigned long long, or unsigned long long int
 *     float
 *     double
 *     long double
 *     _Bool
 *     struct specifier
 *     union specifier
 *     enum specifier
 *     typedef name
 */
INTERNAL Type declaration_specifiers(struct declaration_specifier_info *info)
{
    Type type = {0};
    const Type *tagged;
    enum token_type t;
    enum {
        B_NONE,
        B_VOID,
        B_BOOL,
        B_CHAR,
        B_INT,
        B_FLOAT,
        B_DOUBLE,
        B_ENUM,
        B_AGGREGATE
    } base = 0;
    enum {
        M_NONE,
        M_SHORT,
        M_LONG,
        M_LONG_LONG
    } modifier = 0;
    enum {
        S_NONE,
        S_SIGNED,
        S_UNSIGNED
    } sign = 0;
    enum {
        Q_NONE,
        Q_CONST = 1,
        Q_VOLATILE = 2,
        Q_CONST_VOLATILE = Q_CONST | Q_VOLATILE
    } qual = 0;

    if (info) {
        memset(info, 0, sizeof(*info));
    }

    while (1) {
        t = peek();
        switch (t) {
        case VOID:
            if (base || modifier || sign) goto done;
            next();
            base = B_VOID;
            break;
        case BOOL:
            if (base || modifier || sign) goto done;
            next();
            base = B_BOOL;
            break;
        case CHAR:
            if (base || modifier) goto done;
            next();
            base = B_CHAR;
            break;
        case SHORT:
            if (modifier || (base && base != B_INT)) goto done;
            next();
            modifier = M_SHORT;
            break;
        case INT:
            if (base) goto done;
            next();
            base = B_INT;
            break;
        case LONG:
            if (base || (modifier && modifier != M_LONG)) goto done;
            next();
            if (modifier == M_LONG) {
                modifier = M_LONG_LONG;
            } else {
                modifier = M_LONG;
            }
            break;
        case SIGNED:
            next();
            if (sign == S_SIGNED) {
                error("Duplicate 'signed' specifier.");
            } else if (sign == S_UNSIGNED) {
                error("Conflicting 'signed' and 'unsigned' specifiers.");
            } else {
                sign = S_SIGNED;
            }
            break;
        case UNSIGNED:
            next();
            if (sign == S_UNSIGNED) {
                error("Duplicate 'unsigned' specifier.");
            } else if (sign == S_SIGNED) {
                error("Conflicting 'signed' and 'unsigned' specifiers.");
            } else {
                sign = S_UNSIGNED;
            }
            break;
        case FLOAT:
            if (base || modifier || sign) goto done;
            next();
            base = B_FLOAT;
            break;
        case DOUBLE:
            if (base || (modifier && modifier != M_LONG) || sign) goto done;
            next();
            base = B_DOUBLE;
            break;
        case CONST:
            next();
            qual |= Q_CONST;
            break;
        case VOLATILE:
            next();
            qual |= Q_VOLATILE;
            break;
        case IDENTIFIER:
            if (base || modifier || sign) goto done;
            tagged = get_typedef(access_token(1)->d.string);
            if (!tagged) goto done;
            next();
            type = *tagged;
            base = B_AGGREGATE;
            if (info) {
                info->from_typedef = 1;
            }
            break;
        case UNION:
        case STRUCT:
            if (base || modifier || sign) goto done;
            next();
            type = struct_or_union_declaration(t);
            base = B_AGGREGATE;
            break;
        case ENUM:
            if (base || modifier || sign) goto done;
            next();
            enum_declaration();
            base = B_ENUM;
            break;
        case INLINE:
            next();
            if (!info) {
                error("Unexpected 'inline' specifier.");
            } else if (info->is_inline) {
                error("Multiple 'inline' specifiers.");
            } else {
                info->is_inline = 1;
            }
            break;
        case REGISTER:
            next();
            if (!info) {
                error("Unexpected 'register' specifier.");
            } else if (info->is_register) {
                error("Multiple 'register' specifiers.");
            } else {
                info->is_register = 1;
            }
            break;
        case AUTO:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            next();
            if (!info) {
                error("Unexpected storage class in qualifier list.");
            } else if (info->storage_class) {
                error("Multiple storage class specifiers.");
            } else {
                info->storage_class = t;
            }
            break;
        default:
            goto done;
        }
    }

done:
    switch (base) {
    case B_AGGREGATE:
        break;
    case B_VOID:
        type.type = T_VOID;
        break;
    case B_BOOL:
        type.type = T_BOOL;
        break;
    case B_CHAR:
        type.type = T_CHAR;
        type.is_unsigned = sign == S_UNSIGNED;
        break;
    case B_ENUM:
    case B_NONE:
    case B_INT:
        type.type = T_INT;
        type.is_unsigned = sign == S_UNSIGNED;
        switch (modifier) {
        case M_SHORT:
            type.type = T_SHORT;
            break;
        case M_LONG:
        case M_LONG_LONG:
            type.type = T_LONG;
        default:
            break;
        }
        break;
    case B_FLOAT:
        type.type = T_FLOAT;
        break;
    case B_DOUBLE:
        type.type = T_DOUBLE;
        if (modifier == M_LONG) {
            type.type = T_LDOUBLE;
        }
        break;
    }

    if (qual & Q_CONST)
        type = type_set_const(type);
    if (qual & Q_VOLATILE)
        type = type_set_volatile(type);

    return type;
}

/* Define __func__ as static const char __func__[] = sym->name;
 *
 * Just add the symbol directly as a special string value. No
 * explicit assignment reflected in the IR.
 */
static void define_builtin__func__(String name)
{
    Type type;
    struct symbol *sym;
    size_t len;

    static String func = SHORT_STRING_INIT("__func__");

    assert(current_scope_depth(&ns_ident) == 1);
    if (context.standard >= STD_C99) {
        len = str_len(name);
        type = type_create_array(basic_type__char, len + 1);
        sym = sym_add(&ns_ident, func, type, SYM_LITERAL, LINK_INTERN);
        sym->value.string = name;
    }
}

static void ensure_main_returns_zero(
    struct symbol *sym,
    struct block *block)
{
    static String name = SHORT_STRING_INIT("main");

    assert(is_function(sym->type));
    assert(!sym->n);
    if (context.standard < STD_C99 || !str_eq(name, sym->name))
        return;

    if (!block->has_return_value) {
        block->expr = as_expr(var_int(0));
        block->has_return_value = 1;
    }
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
static struct block *parameter_declaration_list(
    struct definition *def,
    struct block *block,
    Type type)
{
    int i;
    struct symbol sym = {0};
    struct member *param;

    assert(is_function(type));
    assert(current_scope_depth(&ns_ident) == 1);
    assert(!def->symbol);

    sym.type = type;
    def->symbol = &sym;
    while (peek() != '{') {
        block = declaration(def, block);
    }

    def->symbol = NULL;
    for (i = 0; i < nmembers(type); ++i) {
        param = get_member(type, i);
        if (str_is_empty(param->name)) {
            error("Missing parameter name at position %d.", i + 1);
            exit(1);
        }

        assert(!param->sym);
        param->sym = sym_lookup(&ns_ident, param->name);
        if (!param->sym || param->sym->depth != 1) {
            assert(is_type_placeholder(param->type));
            param->type = basic_type__int;
            param->sym = sym_add(&ns_ident,
                param->name,
                param->type,
                SYM_DEFINITION,
                LINK_NONE);
        }
    }

    type_seal(type);
    assert(is_complete(type));
    return block;
}

/* Add function parameters to scope. */
static struct block *make_parameters_visible(
    struct definition *def,
    struct block *block)
{
    int i;
    struct member *param;

    assert(def->symbol);
    assert(is_function(def->symbol->type));
    assert(current_scope_depth(&ns_ident) == 1);

    for (i = 0; i < nmembers(def->symbol->type); ++i) {
        param = get_member(def->symbol->type, i);
        if (str_is_empty(param->name)) {
            error("Missing parameter at position %d.", i + 1);
            exit(1);
        }

        assert(param->sym);
        assert(param->sym->depth == 1);
        assert(!is_type_placeholder(param->type));
        assert(!is_array(param->type));
        sym_make_visible(&ns_ident, param->sym);
        array_push_back(&def->params, param->sym);
    }

    return block;
}

INTERNAL struct block *declare_vla(
    struct definition *def,
    struct block *block,
    struct symbol *sym)
{
    struct symbol *addr;

    assert(is_vla(sym->type));
    addr = sym_create_temporary(type_create_pointer(type_next(sym->type)));
    array_push_back(&def->locals, addr);
    sym->value.vla_address = addr;
    eval_vla_alloc(def, block, sym);
    return block;
}

/*
 * Parse declaration, possibly with initializer. New symbols are added
 * to the symbol table.
 *
 * Cover external declarations, functions, and local declarations
 * (with optional initialization code) inside functions.
 */
static struct block *init_declarator(
    struct definition *def,
    struct block *parent,
    Type base,
    enum symtype symtype,
    enum linkage linkage)
{
    Type type;
    String name = SHORT_STRING_INIT("");
    struct symbol *sym;
    const struct member *param;

    if (linkage == LINK_INTERN && current_scope_depth(&ns_ident) != 0) {
        declarator(def, cfg_block_init(def), base, &type, &name);
    } else {
        parent = declarator(def, parent, base, &type, &name);
    }

    if (str_is_empty(name)) {
        return parent;
    }

    if (symtype == SYM_TYPEDEF) {
        /* */
    } else if (is_function(type)) {
        symtype = SYM_DECLARATION;
        linkage = (linkage == LINK_NONE) ? LINK_EXTERN : linkage;
        if (linkage == LINK_INTERN && current_scope_depth(&ns_ident)) {
            error("Cannot declare static function in block scope.");
            exit(1);
        }
    } else if (is_variably_modified(type)) {
        if (current_scope_depth(&ns_ident) == 0) {
            error("Invalid variably modified type at file scope.");
            exit(1);
        } else if (linkage != LINK_NONE
            && !(is_pointer(type) && linkage == LINK_INTERN))
        {
            error("Invalid linkage for block scoped variably modified type.");
            exit(1);
        }
    }

    if (is_function(type) && !is_complete(type) && peek() != ';') {
        push_scope(&ns_ident);
        parent = parameter_declaration_list(def, parent, type);
        pop_scope(&ns_ident);
    }

    sym = sym_add(&ns_ident, name, type, symtype, linkage);
    switch (current_scope_depth(&ns_ident)) {
    case 0: break;
    case 1: /* Parameters from old-style function definitions. */
        assert(def->symbol);
        param = find_type_member(def->symbol->type, name, NULL);
        if (is_array(type)) {
            sym->type = type_create_pointer(type_next(type));
        }
        if (param && is_type_placeholder(param->type)) {
            ((struct member *) param)->type = sym->type;
        } else {
            error("Invalid parameter declaration of %s.", str_raw(name));
            exit(1);
        }
        break;
    default:
        if (symtype == SYM_DEFINITION) {
            assert(linkage == LINK_NONE);
            array_push_back(&def->locals, sym);
            if (is_vla(type)) {
                parent = declare_vla(def, parent, sym);
            }
        }
        break;
    }

    switch (peek()) {
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
        if (is_vla(sym->type)) {
            error("Variable length array cannot be initialized.");
            exit(1);
        }
        next();
        sym->symtype = SYM_DEFINITION;
        parent = initializer(def, parent, sym);
        assert(size_of(sym->type) > 0);
        if (sym->linkage != LINK_NONE) {
            cfg_define(def, sym);
        }
        break;
    case IDENTIFIER:
    case FIRST(type_specifier):
    case FIRST(type_qualifier):
    case REGISTER:
    case '{':
        if (sym->linkage == LINK_NONE) {
            error("Unexpected linkage for %s.", sym_name(sym));
            exit(1);
        }
        if (is_function(sym->type)) {
            sym->symtype = SYM_DEFINITION;
            cfg_define(def, sym);
            push_scope(&ns_label);
            push_scope(&ns_ident);
            parent = make_parameters_visible(def, parent);
            define_builtin__func__(sym->name);
            parent = block(def, parent);
            ensure_main_returns_zero(sym, parent);
            pop_scope(&ns_label);
            pop_scope(&ns_ident);
            return parent;
        }
    default:
        break;
    }

    if (linkage == LINK_INTERN
        || (is_function(sym->type) && sym->symtype != SYM_DEFINITION))
    {
        type_clean_prototype(sym->type);
    }

    return parent;
}

static void static_assertion(void)
{
    struct var val;
    String message;

    consume(STATIC_ASSERT);
    consume('(');

    val = constant_expression();
    consume(',');
    consume(STRING);
    message = access_token(0)->d.string;

    if (val.kind != IMMEDIATE || !is_integer(val.type)) {
        error("Expression in static assertion must be an integer constant.");
        exit(1);
    }

    if (val.imm.i == 0) {
        error(str_raw(message));
        exit(1);
    }

    consume(')');
}

/*
 * Parse a declaration list, beginning with a base set of specifiers,
 * followed by a list of declarators.
 *
 * Each new global declaration is assigned a clean 'struct definition'
 * object, which might get filled with initialization code, or the body
 * of a function.
 *
 * Terminate on hitting a function definition, otherwise read until the
 * end of statement.
 */
INTERNAL struct block *declaration(
    struct definition *def,
    struct block *parent)
{
    Type base, type;
    enum symtype symtype;
    enum linkage linkage;
    struct definition *decl;
    struct symbol *sym;
    struct declaration_specifier_info info;

    if (peek() == STATIC_ASSERT) {
        static_assertion();
        consume(';');
        return parent;
    }

    base = declaration_specifiers(&info);
    switch (info.storage_class) {
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

    switch (peek()) {
    case '*':
    case '(':
    case IDENTIFIER:
        break;
    default:
        consume(';');
        return parent;
    }

    while (1) {
        type = base;
        if (info.from_typedef && is_array(base) && !is_complete(base)) {
            type = type_next(base);
            type = type_create_incomplete(type);
            type = type_apply_qualifiers(type, base);
            assert(type_equal(type, base));
        }

        if (linkage == LINK_INTERN || linkage == LINK_EXTERN) {
            decl = cfg_init();
            init_declarator(decl, decl->body, type, symtype, linkage);
            if (!decl->symbol) {
                cfg_discard(decl);
            } else if (is_function(decl->symbol->type)) {
                if (info.is_inline) {
                    sym = (struct symbol *) decl->symbol;
                    sym->inlined = 1;
                    sym->referenced |= info.storage_class == EXTERN;
                }
                return parent;
            }
        } else {
            parent = init_declarator(def, parent, type, symtype, linkage);
        }

        if (!try_consume(','))
            break;
    }

    consume(';');
    return parent;
}
