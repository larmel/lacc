#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "builtin.h"
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

/*
 * Return 1 iff expression is a constant.
 *
 * No actual evaluation is performed.
 */
static struct block *parse__builtin_constant_p(
    struct definition *def,
    struct block *block)
{
    struct var v;
    struct block *head, *tail;

    head = cfg_block_init(NULL);
    consume('(');
    tail = assignment_expression(NULL, head);
    consume(')');

    v = var_int(tail == head && is_immediate(tail->expr));
    block->expr = as_expr(v);
    return block;
}

/*
 * Parse call to builtin symbol __builtin_va_start, which is the result
 * of calling va_start(arg, s). Return type depends on second input
 * argument.
 */
static struct block *parse__builtin_va_start(
    struct definition *def,
    struct block *block)
{
    Type type;
    String name;
    const struct member *mb;
    const struct symbol *sym;

    consume('(');
    block = assignment_expression(def, block);
    consume(',');
    consume(IDENTIFIER);

    name = access_token(0)->d.string;
    sym = sym_lookup(&ns_ident, name);
    if (!sym) {
        error("Undefined symbol '%s'.", str_raw(name));
        exit(1);
    }

    type = def->symbol->type;
    if (!is_vararg(type)) {
        error("Function must be vararg to use va_start.");
        exit(1);
    }

    mb = get_member(type, nmembers(type) - 1);
    if (!str_eq(mb->name, sym->name) || sym->depth != 1) {
        error("Expected last function argument %s as va_start argument.",
            str_raw(mb->name));
        exit(1);
    }

    consume(')');
    eval__builtin_va_start(block, block->expr);
    return block;
}

/*
 * Parse call to builtin symbol __builtin_va_arg, which is the result of
 * calling va_arg(arg, T). Return type depends on second input argument.
 */
static struct block *parse__builtin_va_arg(
    struct definition *def,
    struct block *block)
{
    struct var value;
    Type type;

    consume('(');
    block = assignment_expression(def, block);
    value = eval(def, block, block->expr);
    consume(',');
    type = declaration_specifiers(NULL);
    if (peek() != ')') {
        block = declarator(def, block, type, &type, NULL);
    }

    consume(')');
    block->expr = eval_va_arg(def, block, value, type);
    return block;
}

/*
 * Implement alloca as a normal VLA.
 *
 *   void *ptr = alloca(n + 1);
 *
 * is translated to
 *
 *   size_t len = n + 1;
 *   char sym[len];
 *   void *ptr = (void *) sym;
 *
 */
static struct block *parse__builtin_alloca(
    struct definition *def,
    struct block *block)
{
    struct var t1;
    struct symbol *sym;

    consume('(');
    block = assignment_expression(def, block);
    consume(')');

    t1 = create_var(def, basic_type__unsigned_long);
    eval_assign(def, block, t1, block->expr);

    sym = sym_create_temporary(type_create_vla(basic_type__char, t1.value.symbol));
    array_push_back(&def->locals, sym);

    block = declare_vla(def, block, sym);
    block->expr = eval_cast(def, block, var_direct(sym),
        type_create_pointer(basic_type__void));
    return block;
}

/*
 * Construct the type definition for va_list:
 *
 *   typedef struct {
 *       unsigned int gp_offset;
 *       unsigned int fp_offset;
 *       void *overflow_arg_area;
 *       void *reg_save_area;
 *   } __builtin_va_list[1];
 *
 */
static void define__builtin_va_list(void)
{
    Type t, v;

    v = type_create_pointer(basic_type__void);

    t = type_create(T_STRUCT);
    type_add_member(t, str_c("gp_offset"), basic_type__unsigned_int);
    type_add_member(t, str_c("fp_offset"), basic_type__unsigned_int);
    type_add_member(t, str_c("overflow_arg_area"), v);
    type_add_member(t, str_c("reg_save_area"), v);
    type_seal(t);

    t = type_create_array(t, 1);
    sym_add(&ns_ident, str_c("__builtin_va_list"), t, SYM_TYPEDEF, LINK_NONE);
}

/* Save memcpy reference for backend. */
INTERNAL const struct symbol *decl_memcpy = NULL;

/*
 * Code generation uses memcpy when dealing with large blocks of data,
 * so we need a declaration visible in the symbol table.
 *
 *   void *memcpy(void *dest, const void *src, unsigned long n);
 *
 * If the compiler is invoked with -nostdinc, then it should not be
 * included, and backend will use other means to copy.
 */
static void declare_memcpy(void)
{
    Type t, dest, src;

    if (context.nostdinc)
        return;

    dest = type_create_pointer(basic_type__void);
    src = type_set_const(basic_type__void);
    src = type_create_pointer(src);

    t = type_create_function(dest);
    type_add_member(t, str_c("dest"), dest);
    type_add_member(t, str_c("src"), src);
    type_add_member(t, str_c("n"), basic_type__unsigned_long);
    type_seal(t);

    decl_memcpy =
        sym_add(&ns_ident, str_c("memcpy"), t, SYM_DECLARATION, LINK_EXTERN);
}

INTERNAL void register_builtins(void)
{
    define__builtin_va_list();
    sym_create_builtin(str_c("__builtin_alloca"), parse__builtin_alloca);
    sym_create_builtin(str_c("__builtin_va_start"), parse__builtin_va_start);
    sym_create_builtin(str_c("__builtin_va_arg"), parse__builtin_va_arg);
    sym_create_builtin(str_c("__builtin_constant_p"), parse__builtin_constant_p);

    declare_memcpy();
}
