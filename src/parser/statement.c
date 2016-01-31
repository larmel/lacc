#include "statement.h"
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "symtab.h"
#include "type.h"
#include <lacc/token.h>
#include <lacc/cli.h>

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

/* Store reference to top of loop, for resolving break and continue. Use call
 * stack to keep track of depth, backtracking to the old value.
 */
static struct block
    *break_target,
    *continue_target;

/* Keep track of nested switch statements and their case labels.
 */
static struct switch_context {
    struct block *default_label;
    struct block **case_label;
    struct var *case_value;
    int n;
} *switch_ctx;

static void add_switch_case(struct block *label, struct var value)
{
    struct switch_context *ctx = switch_ctx;

    ctx->n++;
    ctx->case_label =
        realloc(ctx->case_label, ctx->n * sizeof(*ctx->case_label));
    ctx->case_value =
        realloc(ctx->case_value, ctx->n * sizeof(*ctx->case_value));

    ctx->case_label[ctx->n - 1] = label;
    ctx->case_value[ctx->n - 1] = value;
}

static void free_switch_context(struct switch_context *ctx)
{
    assert(ctx);
    if (ctx->n) {
        free(ctx->case_label);
        free(ctx->case_value);
    }
    free(ctx);
}

static int is_immediate_true(struct var e)
{
    return e.kind == IMMEDIATE && is_integer(e.type) && e.imm.i;
}

static int is_immediate_false(struct var e)
{
    return e.kind == IMMEDIATE && is_integer(e.type) && !e.imm.i;
}

static struct block *if_statement(struct block *parent)
{
    struct block
        *right = cfg_block_init(),
        *next  = cfg_block_init();

    consume(IF);
    consume('(');
    parent = expression(parent);
    consume(')');
    if (is_immediate_true(parent->expr)) {
        parent->jump[0] = right;
    } else if (is_immediate_false(parent->expr)) {
        parent->jump[0] = next;
    } else {
        parent->jump[0] = next;
        parent->jump[1] = right;
    }

    right = statement(right);
    right->jump[0] = next;
    if (peek().token == ELSE) {
        struct block *left = cfg_block_init();
        consume(ELSE);
        parent->jump[0] = left;
        left = statement(left);
        left->jump[0] = next;
    }

    return next;
}

static struct block *do_statement(struct block *parent)
{
    struct block
        *top = cfg_block_init(),
        *body,
        *cond = cfg_block_init(),
        *tail,
        *next = cfg_block_init();

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, cond);
    parent->jump[0] = top;

    consume(DO);
    body = statement(top);
    body->jump[0] = cond;
    consume(WHILE);
    consume('(');
    tail = expression(cond);
    consume(')');
    if (is_immediate_true(tail->expr)) {
        tail->jump[0] = top;
    } else if (is_immediate_false(tail->expr)) {
        tail->jump[0] = next;
    } else {
        tail->jump[0] = next;
        tail->jump[1] = top;
    }

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *while_statement(struct block *parent)
{
    struct block
        *top = cfg_block_init(),
        *cond,
        *body = cfg_block_init(),
        *next = cfg_block_init();

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, top);
    parent->jump[0] = top;

    consume(WHILE);
    consume('(');
    cond = expression(top);
    consume(')');
    if (is_immediate_true(cond->expr)) {
        cond->jump[0] = body;
    } else if (is_immediate_false(cond->expr)) {
        cond->jump[0] = next;
    } else {
        cond->jump[0] = next;
        cond->jump[1] = body;
    }

    body = statement(body);
    body->jump[0] = top;

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *for_statement(struct block *parent)
{
    struct block
        *top = cfg_block_init(),
        *body = cfg_block_init(),
        *increment = cfg_block_init(),
        *next = cfg_block_init();

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, increment);

    consume(FOR);
    consume('(');
    if (peek().token != ';') {
        parent = expression(parent);
    }

    consume(';');
    if (peek().token != ';') {
        parent->jump[0] = top;
        top = expression(top);
        if (is_immediate_true(top->expr)) {
            top->jump[0] = body;
        } else if (is_immediate_false(top->expr)) {
            top->jump[0] = next;
        } else {
            top->jump[0] = next;
            top->jump[1] = body;
        }
        top = (struct block *) parent->jump[0];
    } else {
        /* Infinite loop */
        parent->jump[0] = body;
        top = body;
    }

    consume(';');
    if (peek().token != ')') {
        expression(increment)->jump[0] = top;
    }

    consume(')');
    body = statement(body);
    body->jump[0] = increment;

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *switch_statement(struct block *parent)
{
    struct block
        *body = cfg_block_init(),
        *last,
        *next = cfg_block_init();

    struct switch_context *old_switch_ctx;
    struct block *old_break_target;

    set_break_target(old_break_target, next);
    old_switch_ctx = switch_ctx;
    switch_ctx = calloc(1, sizeof(*switch_ctx));

    consume(SWITCH);
    consume('(');
    parent = expression(parent);
    consume(')');
    last = statement(body);
    last->jump[0] = next;

    if (!switch_ctx->n && !switch_ctx->default_label) {
        parent->jump[0] = next;
    } else {
        int i;
        struct block *cond = parent;

        for (i = 0; i < switch_ctx->n; ++i) {
            struct block *prev_cond = cond;
            struct block *label = switch_ctx->case_label[i];
            struct var value = switch_ctx->case_value[i];

            cond = cfg_block_init();
            cond->expr = eval_expr(cond, IR_OP_EQ, value, parent->expr);
            cond->jump[1] = label;
            prev_cond->jump[0] = cond;
        }

        cond->jump[0] = (switch_ctx->default_label) ?
            switch_ctx->default_label : next;
    }

    free_switch_context(switch_ctx);
    restore_break_target(old_break_target);
    switch_ctx = old_switch_ctx;
    return next;
}

struct block *statement(struct block *parent)
{
    const struct symbol *sym;
    struct token tok;

    switch ((tok = peek()).token) {
    case ';':
        consume(';');
        break;
    case '{':
        parent = block(parent);
        break;
    case IF:
        parent = if_statement(parent);
        break;
    case DO:
        parent = do_statement(parent);
        break;
    case WHILE:
        parent = while_statement(parent);
        break;
    case FOR:
        parent = for_statement(parent);
        break;
    case GOTO:
        consume(GOTO);
        consume(IDENTIFIER);
        /* todo */
        consume(';');
        break;
    case CONTINUE:
    case BREAK:
        next();
        parent->jump[0] =
            (tok.token == CONTINUE) ? continue_target : break_target;
        consume(';');
        /* Return orphan node, which is dead code unless there is a label and a
         * goto statement. */
        parent = cfg_block_init(); 
        break;
    case RETURN:
        consume(RETURN);
        sym = current_func()->symbol;
        if (!is_void(sym->type.next)) {
            parent = expression(parent);
            parent->expr = eval_return(parent, sym->type.next);
        }
        consume(';');
        parent = cfg_block_init(); /* orphan */
        break;
    case SWITCH:
        parent = switch_statement(parent);
        break;
    case CASE:
        consume(CASE);
        if (!switch_ctx) {
            error("Stray 'case' label, must be inside a switch statement.");
        } else {
            struct block *next = cfg_block_init();
            struct var expr = constant_expression();
            consume(':');
            add_switch_case(next, expr);
            parent->jump[0] = next;
            next = statement(next);
            parent = next;
        }
        break;
    case DEFAULT:
        consume(DEFAULT);
        consume(':');
        if (!switch_ctx) {
            error("Stray 'default' label, must be inside a switch statement.");
        } else if (switch_ctx->default_label) {
            error("Multiple 'default' labels inside the same switch.");
        } else {
            struct block *next = cfg_block_init();
            parent->jump[0] = next;
            switch_ctx->default_label = next;
            next = statement(next);
            parent = next;
        }
        break;
    case IDENTIFIER:
        sym = sym_lookup(&ns_ident, tok.strval.str);
        if (sym && sym->symtype == SYM_TYPEDEF) {
            parent = declaration(parent);
            break;
        }
        /* fallthrough */
    case INTEGER_CONSTANT:
    case STRING:
    case '*':
    case '(':
        parent = expression(parent);
        consume(';');
        break;
    default:
        parent = declaration(parent);
        break;
    }

    return parent;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
struct block *block(struct block *parent)
{
    consume('{');
    push_scope(&ns_ident);
    push_scope(&ns_tag);
    while (peek().token != '}') {
        parent = statement(parent);
    }
    consume('}');
    pop_scope(&ns_tag);
    pop_scope(&ns_ident);
    return parent;
}
