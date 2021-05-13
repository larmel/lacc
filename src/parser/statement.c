#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "statement.h"
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/array.h>
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

#define set_break_target(old, brk) \
    old = break_target; \
    break_target = brk;

#define set_continue_target(old, cont) \
    old = continue_target; \
    continue_target = cont;

#define restore_break_target(old) \
    break_target = old;

#define restore_continue_target(old) \
    continue_target = old;

/*
 * Store reference to top of loop, for resolving break and continue. Use
 * call stack to keep track of depth, backtracking to the old value.
 */
static struct block
    *break_target,
    *continue_target;

struct switch_case {
    struct block *label;
    struct var value;
};

struct switch_context {
    struct block *default_label;
    array_of(struct switch_case) cases;
};

/*
 * Keep track of nested switch statements and their case labels. This
 * reference always points to the current context, and backtracking is
 * managed recursively by switch_statement.
 */
static struct switch_context *switch_context;

static void add_switch_case(struct block *label, struct var value)
{
    struct switch_case sc;

    sc.label = label;
    sc.value = value;
    array_push_back(&switch_context->cases, sc);
}

static void free_switch_context(struct switch_context *ctx)
{
    assert(ctx);
    array_clear(&ctx->cases);
    free(ctx);
}

static struct block *expression_statement(
    struct definition *def,
    struct block *block)
{
    block = expression(def, block);
    block->expr = eval_expression_statement(def, block, block->expr);
    return block;
}

static struct block *if_statement(
    struct definition *def,
    struct block *parent)
{
    int b;
    struct block
        *right = cfg_block_init(def), *left,
        *next  = cfg_block_init(def);

    consume(IF);
    consume('(');
    parent = expression(def, parent);
    parent = scalar(def, parent, "If expression");
    consume(')');

    b = immediate_bool(parent->expr);
    if (b == 1) {
        parent->jump[0] = right;
    } else if (b == 0) {
        parent->jump[0] = next;
    } else {
        assert(b == -1);
        parent->jump[0] = next;
        parent->jump[1] = right;
    }

    right = statement(def, right);
    right->jump[0] = next;
    if (try_consume(ELSE)) {
        left = cfg_block_init(def);
        if (b != 1) {
            /*
             * This block will be an orphan if the branch is immediate
             * taken true branch. Still need to evaluate the expression
             * here though.
             */
            parent->jump[0] = left;
        }

        left = statement(def, left);
        left->jump[0] = next;
    }

    return next;
}

static struct block *do_statement(
    struct definition *def,
    struct block *parent)
{
    int b;
    struct block
        *top = cfg_block_init(def),
        *body,
        *cond = cfg_block_init(def),
        *tail,
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, cond);
    parent->jump[0] = top;

    consume(DO);
    body = statement(def, top);
    body->jump[0] = cond;
    consume(WHILE);
    consume('(');
    tail = expression(def, cond);
    tail = scalar(def, tail, "While expression");
    consume(')');

    b = immediate_bool(tail->expr);
    if (b == 1) {
        tail->jump[0] = top;
    } else if (b == 0) {
        tail->jump[0] = next;
    } else {
        assert(b == -1);
        tail->jump[0] = next;
        tail->jump[1] = top;
    }

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *while_statement(
    struct definition *def,
    struct block *parent)
{
    int b;
    struct block
        *top = cfg_block_init(def),
        *cond,
        *body = cfg_block_init(def),
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, top);
    parent->jump[0] = top;

    consume(WHILE);
    consume('(');
    cond = expression(def, top);
    cond = scalar(def, cond, "While expression");
    consume(')');

    b = immediate_bool(cond->expr);
    if (b == 1) {
        cond->jump[0] = body;
    } else if (b == 0) {
        cond->jump[0] = next;
    } else {
        assert(b == -1);
        cond->jump[0] = next;
        cond->jump[1] = body;
    }

    body = statement(def, body);
    body->jump[0] = top;

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *for_statement(
    struct definition *def,
    struct block *parent)
{
    int declared, b;
    const struct symbol *sym;
    struct block
        *top = cfg_block_init(def),
        *body = cfg_block_init(def),
        *increment = cfg_block_init(def),
        *tail = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, tail);

    declared = 0;
    consume(FOR);
    consume('(');
    switch (peek()) {
    case IDENTIFIER:
        sym = sym_lookup(&ns_ident, access_token(1)->d.string);
        if (!sym || sym->symtype != SYM_TYPEDEF) {
            parent = expression_statement(def, parent);
            consume(';');
            break;
        }
    case FIRST(type_name):
        declared = 1;
        push_scope(&ns_ident);
        parent = declaration(def, parent);
        break;
    default:
        parent = expression_statement(def, parent);
    case ';':
        next();
        break;
    }

    if (peek() != ';') {
        parent->jump[0] = top;
        top = expression(def, top);
        top = scalar(def, top, "Controlling expression");
        b = immediate_bool(top->expr);
        if (b == 1) {
            top->jump[0] = body;
        } else if (b == 0) {
            top->jump[0] = tail;
        } else {
            assert(b == -1);
            top->jump[0] = tail;
            top->jump[1] = body;
        }

        top = (struct block *) parent->jump[0];
    } else {
        /* Infinite loop. */
        parent->jump[0] = body;
        top = body;
    }

    consume(';');
    if (!try_consume(')')) {
        expression_statement(def, increment)->jump[0] = top;
        consume(')');
        set_continue_target(old_continue_target, increment);
        body = statement(def, body);
        body->jump[0] = increment;
    } else {
        set_continue_target(old_continue_target, top);
        body = statement(def, body);
        body->jump[0] = top;
    }

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    if (declared) {
        pop_scope(&ns_ident);
    }

    return tail;
}

static struct block *switch_statement(
    struct definition *def,
    struct block *parent)
{
    int i;
    struct var value;
    struct switch_case sc;
    struct block
        *cond,
        *prev_cond,
        *body = cfg_block_init(def),
        *last,
        *next = cfg_block_init(def);

    struct switch_context *old_switch_ctx;
    struct block *old_break_target;

    set_break_target(old_break_target, next);
    old_switch_ctx = switch_context;
    switch_context = calloc(1, sizeof(*switch_context));

    consume(SWITCH);
    consume('(');
    parent = expression(def, parent);
    value = eval(def, parent, parent->expr);
    parent->expr = as_expr(value);
    if (!is_integer(value.type)) {
        error("Switch expression must have integer type, was %t.", value.type);
        exit(1);
    }

    consume(')');
    last = statement(def, body);
    last->jump[0] = next;

    if (!array_len(&switch_context->cases) && !switch_context->default_label) {
        parent->jump[0] = next;
    } else {
        cond = parent;
        for (i = 0; i < array_len(&switch_context->cases); ++i) {
            prev_cond = cond;
            sc = array_get(&switch_context->cases, i);
            cond = cfg_block_init(def);
            cond->expr = eval_cmp_eq(def, cond, sc.value, value);
            cond->jump[1] = sc.label;
            prev_cond->jump[0] = cond;
        }

        cond->jump[0] = (switch_context->default_label) ?
            switch_context->default_label : next;
    }

    free_switch_context(switch_context);
    restore_break_target(old_break_target);
    switch_context = old_switch_ctx;
    return next;
}

/*
 * Parse operands to __asm__ expressions.
 *
 * Of the form "=r" (*a).
 *
 * Produce operands as 'struct var' objects.
 */
static struct asm_operand asm_operand(
    struct definition *def,
    struct block **block,
    struct block *writeback)
{
    struct asm_operand op = {0};
    struct var var, tmp;
    int force_register;
    const char *str;

    if (try_consume('[')) {
        consume(IDENTIFIER);
        op.alias = access_token(0)->d.string;
        consume(']');
    }

    consume(STRING);
    op.constraint = access_token(0)->d.string;
    str = str_raw(op.constraint);

    consume('(');
    *block = conditional_expression(def, *block);
    consume(')');

    var = eval(def, *block, (*block)->expr);
    if (str[0] == '=' || str[0] == '+') {
        if (!writeback) {
            error("Input operand cannot be writeable.");
            exit(1);
        }
        if (!var.lvalue) {
            error("Output operand must be lvalue.");
            exit(1);
        }
    } else if (writeback) {
        error("Output operand constraint must begin with '=' or '+'.");
        exit(1);
    } else {
        var = rvalue(def, *block, var);
    }

    force_register = strchr(str, 'r') && !strchr(str, 'm');

    if (force_register && (var.kind != DIRECT || !is_temporary(var.symbol))) {
        tmp = create_var(def, var.type);
        op.variable = tmp;
        if (!writeback || str[0] == '+') {
            eval_assign(def, *block, tmp, as_expr(var));
        }
        if (writeback) {
            eval_assign(def, writeback, var, as_expr(tmp));
        }
    } else if (var.kind == DEREF && !is_temporary(var.symbol)) {
        var = eval_addr(def, *block, var);
        tmp = create_var(def, var.type);
        eval_assign(def, *block, tmp, as_expr(var));
        op.variable = eval_deref(def, *block, tmp);
    } else {
        op.variable = var;
    }

    return op;
}

/*
 * Replicate GCC behavior, allowing 'volatile', 'volatile goto', and
 * just 'goto'.
 *
 * The goto qualifier is required when assembly template contains jumps.
 */
static void asm_statement_qualifiers(int *is_volatile, int *is_goto)
{
    *is_volatile = try_consume(VOLATILE);
    *is_goto = try_consume(GOTO);
}

/*
 * Parse __asm__ statement.
 *
 * Read input and output operand constraints, and make sure all are
 * convertet to DIRECT variables if specified to be in register.
 *
 * Converted variables that result in temporaries are written back to
 * the actual location in a separate block.
 */
static struct block *asm_statement(
    struct definition *def,
    struct block *block)
{
    struct asm_operand op;
    struct asm_statement st = {0};
    struct symbol *sym;
    struct block *writeback;
    int is_volatile, is_goto;

    writeback = cfg_block_init(def);
    asm_statement_qualifiers(&is_volatile, &is_goto);
    consume('(');
    consume(STRING);
    st.template = access_token(0)->d.string;

    consume(':');
    while (!is_goto && peek() != ':') {
        op = asm_operand(def, &block, writeback);
        array_push_back(&st.operands, op);
        if (!try_consume(','))
            break;
    }

    if (!try_consume(':'))
        goto end;

    while (peek() != ':' && peek() != ')') {
        op = asm_operand(def, &block, NULL);
        array_push_back(&st.operands, op);
        if (!try_consume(','))
            break;
    }

    if (!try_consume(':'))
        goto end;

    while (try_consume(STRING)) {
        array_push_back(&st.clobbers, access_token(0)->d.string);
        if (!try_consume(','))
            break;
    }

    if (is_goto) {
        consume(':');
        while (1) {
            consume(IDENTIFIER);
            sym = sym_add(
                &ns_label,
                access_token(0)->d.string,
                basic_type__void,
                SYM_TENTATIVE,
                LINK_INTERN);
            if (!sym->value.label) {
                sym->value.label = cfg_block_init(def);
            }

            array_push_back(&st.targets, sym->value.label);
            if (!try_consume(','))
                break;
        }
    }

end:
    consume(')');
    array_push_back(&def->asm_statements, st);
    ir_asm(block, array_len(&def->asm_statements) - 1);
    block->jump[0] = writeback;
    return writeback;
}

INTERNAL struct block *statement(
    struct definition *def,
    struct block *parent)
{
    struct symbol *sym;
    String str;

    switch (peek()) {
    case ';':
        next();
        break;
    case '{':
        parent = block(def, parent);
        break;
    case IF:
        parent = if_statement(def, parent);
        break;
    case DO:
        parent = do_statement(def, parent);
        consume(';');
        break;
    case WHILE:
        parent = while_statement(def, parent);
        break;
    case FOR:
        parent = for_statement(def, parent);
        break;
    case GOTO:
        next();
        consume(IDENTIFIER);
        sym = sym_add(
            &ns_label,
            access_token(0)->d.string,
            basic_type__void,
            SYM_TENTATIVE,
            LINK_INTERN);
        if (!sym->value.label) {
            sym->value.label = cfg_block_init(def);
        }
        parent->jump[0] = sym->value.label;
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        consume(';');
        break;
    case CONTINUE:
        next();
        parent->jump[0] = continue_target;
        consume(';');
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        break;
    case BREAK:
        next();
        parent->jump[0] = break_target;
        consume(';');
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        break;
    case RETURN:
        next();
        if (!is_void(type_next(def->symbol->type))) {
            parent = expression(def, parent);
            parent->expr = eval_return(def, parent);
        }
        consume(';');
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        break;
    case SWITCH:
        parent = switch_statement(def, parent);
        break;
    case CASE:
        next();
        if (!switch_context) {
            error("Stray 'case' label, must be inside a switch statement.");
        } else {
            struct block *next = cfg_block_init(def);
            struct var expr = constant_expression();
            consume(':');
            add_switch_case(next, expr);
            parent->jump[0] = next;
            next = statement(def, next);
            parent = next;
        }
        break;
    case DEFAULT:
        next();
        consume(':');
        if (!switch_context) {
            error("Stray 'default' label, must be inside a switch statement.");
        } else if (switch_context->default_label) {
            error("Multiple 'default' labels inside the same switch.");
        } else {
            struct block *next = cfg_block_init(def);
            parent->jump[0] = next;
            switch_context->default_label = next;
            next = statement(def, next);
            parent = next;
        }
        break;
    case ASM:
        next();
        parent = asm_statement(def, parent);
        consume(';');
        break;
    case IDENTIFIER:
        str = access_token(1)->d.string;
        if (peekn(2) == ':') {
            consume(IDENTIFIER);
            sym = sym_lookup(&ns_label, str);
            if (sym && sym->symtype == SYM_DEFINITION) {
                error("Duplicate label '%s'.", str_raw(str));
            } else {
                sym = sym_add(
                    &ns_label,
                    str,
                    basic_type__void,
                    SYM_DEFINITION,
                    LINK_INTERN);
                if (!sym->value.label) {
                    assert(!sym->referenced);
                    sym->value.label = cfg_block_init(def);
                }
                parent->jump[0] = sym->value.label;
                parent = sym->value.label;
            }
            consume(':');
            return statement(def, parent);
        }
        sym = sym_lookup(&ns_ident, str);
        if (sym && sym->symtype == SYM_TYPEDEF) {
            parent = declaration(def, parent);
            break;
        }
        /* Fallthrough. */
    case NUMBER:
    case STRING:
    case '*':
    case '(':
    case INCREMENT:
    case DECREMENT:
        parent = expression_statement(def, parent);
        consume(';');
        break;
    default:
        parent = declaration(def, parent);
        break;
    }

    return parent;
}

/*
 * Treat statements and declarations equally, allowing declarations in
 * between statements as in modern C. Called compound-statement in K&R.
 */
INTERNAL struct block *block(struct definition *def, struct block *parent)
{
    consume('{');
    push_scope(&ns_ident);
    push_scope(&ns_tag);
    while (!try_consume('}')) {
        parent = statement(def, parent);
    }

    pop_scope(&ns_tag);
    pop_scope(&ns_ident);
    return parent;
}
