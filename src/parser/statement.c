#include "statement.h"
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "symtab.h"
#include "type.h"
#include <lacc/array.h>
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

/* Store reference to top of loop, for resolving break and continue.
 * Use call stack to keep track of depth, backtracking to the old value.
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

/* Keep track of nested switch statements and their case labels. This
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

static int is_immediate_true(struct var e)
{
    return e.kind == IMMEDIATE && is_integer(e.type) && e.imm.i;
}

static int is_immediate_false(struct var e)
{
    return e.kind == IMMEDIATE && is_integer(e.type) && !e.imm.i;
}

static struct block *if_statement(
    struct definition *def,
    struct block *parent)
{
    struct block
        *right = cfg_block_init(def),
        *next  = cfg_block_init(def);

    consume(IF);
    consume('(');
    parent = expression(def, parent);
    consume(')');
    if (is_immediate_true(parent->expr)) {
        parent->jump[0] = right;
    } else if (is_immediate_false(parent->expr)) {
        parent->jump[0] = next;
    } else {
        parent->jump[0] = next;
        parent->jump[1] = right;
    }

    right = statement(def, right);
    right->jump[0] = next;
    if (peek().token == ELSE) {
        struct block *left = cfg_block_init(def);
        consume(ELSE);
        parent->jump[0] = left;
        left = statement(def, left);
        left->jump[0] = next;
    }

    return next;
}

static struct block *do_statement(
    struct definition *def,
    struct block *parent)
{
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

static struct block *while_statement(
    struct definition *def,
    struct block *parent)
{
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
    consume(')');
    if (is_immediate_true(cond->expr)) {
        cond->jump[0] = body;
    } else if (is_immediate_false(cond->expr)) {
        cond->jump[0] = next;
    } else {
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
    struct block
        *top = cfg_block_init(def),
        *body = cfg_block_init(def),
        *increment = cfg_block_init(def),
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, increment);

    consume(FOR);
    consume('(');
    if (peek().token != ';') {
        parent = expression(def, parent);
    }

    consume(';');
    if (peek().token != ';') {
        parent->jump[0] = top;
        top = expression(def, top);
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
        expression(def, increment)->jump[0] = top;
    }

    consume(')');
    body = statement(def, body);
    body->jump[0] = increment;

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *switch_statement(
    struct definition *def,
    struct block *parent)
{
    struct block
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
    consume(')');
    last = statement(def, body);
    last->jump[0] = next;

    if (!array_len(&switch_context->cases) && !switch_context->default_label) {
        parent->jump[0] = next;
    } else {
        int i;
        struct block *cond = parent;

        for (i = 0; i < array_len(&switch_context->cases); ++i) {
            struct block *prev_cond = cond;
            struct switch_case sc = array_get(&switch_context->cases, i);

            cond = cfg_block_init(def);
            cond->expr = eval_expr(def, cond, IR_OP_EQ, sc.value, parent->expr);
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

struct block *statement(struct definition *def, struct block *parent)
{
    struct symbol *sym;
    struct token tok;

    switch ((tok = peek()).token) {
    case ';':
        consume(';');
        break;
    case '{':
        parent = block(def, parent);
        break;
    case IF:
        parent = if_statement(def, parent);
        break;
    case DO:
        parent = do_statement(def, parent);
        break;
    case WHILE:
        parent = while_statement(def, parent);
        break;
    case FOR:
        parent = for_statement(def, parent);
        break;
    case GOTO:
        consume(GOTO);
        tok = consume(IDENTIFIER);
        sym = sym_add(
            &ns_label,
            tok.d.string,
            &basic_type__void,
            SYM_TENTATIVE,
            LINK_INTERN);
        if (!sym->label_value) {
            sym->label_value = cfg_block_init(def);
        }
        parent->jump[0] = sym->label_value;
        parent = cfg_block_init(def); /* orphan, unless labeled */
        consume(';');
        break;
    case CONTINUE:
    case BREAK:
        next();
        parent->jump[0] =
            (tok.token == CONTINUE) ? continue_target : break_target;
        consume(';');
        parent = cfg_block_init(def); /* orphan, unless labeled */
        break;
    case RETURN:
        consume(RETURN);
        if (!is_void(def->symbol->type.next)) {
            parent = expression(def, parent);
            parent->expr = eval_return(def, parent);
        }
        consume(';');
        parent = cfg_block_init(def); /* orphan, unless labeled */
        break;
    case SWITCH:
        parent = switch_statement(def, parent);
        break;
    case CASE:
        consume(CASE);
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
        consume(DEFAULT);
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
    case IDENTIFIER:
        if (peekn(2).token == ':') {
            consume(IDENTIFIER);
            sym = sym_lookup(&ns_label, tok.d.string);
            if (sym && sym->symtype == SYM_DEFINITION) {
                error("Duplicate label '%s'.", str_raw(tok.d.string));
            } else {
                sym = sym_add(
                    &ns_label,
                    tok.d.string,
                    &basic_type__void,
                    SYM_DEFINITION,
                    LINK_INTERN);
                if (!sym->label_value) {
                    assert(!sym->referenced);
                    sym->label_value = cfg_block_init(def);
                }
                parent->jump[0] = sym->label_value;
                parent = sym->label_value;
            }
            consume(':');
            break;
        } else {
            sym = sym_lookup(&ns_ident, tok.d.string);
            if (sym && sym->symtype == SYM_TYPEDEF) {
                parent = declaration(def, parent);
                break;
            }
        }
        /* fallthrough */
    case NUMBER:
    case STRING:
    case '*':
    case '(':
        parent = expression(def, parent);
        consume(';');
        break;
    default:
        parent = declaration(def, parent);
        break;
    }

    return parent;
}

/* Treat statements and declarations equally, allowing declarations in
 * between statements as in modern C. Called compound-statement in K&R.
 */
struct block *block(struct definition *def, struct block *parent)
{
    consume('{');
    push_scope(&ns_ident);
    push_scope(&ns_tag);
    while (peek().token != '}') {
        parent = statement(def, parent);
    }
    consume('}');
    pop_scope(&ns_tag);
    pop_scope(&ns_ident);
    return parent;
}
