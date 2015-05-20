#include "error.h"
#include "input.h"
#include "macro.h"
#include "preprocess.h"
#include "util/map.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Buffer of preprocessed tokens, ready to be consumed by the parser. Configured
 * to hold at least K tokens, enabling LL(K) parsing. For the K&R grammar, it is
 * sufficient to have K = 2. */
static struct toklist lookahead;
static const int K = 2;

static void add_token(struct token t)
{
    extern int VERBOSE;

    toklist_push_back(&lookahead, t);

    if (VERBOSE) {
        debug_output_token(t);
    }
}

/* Push and pop branch conditions for #if, #elif and #endif.
 */
static struct {
    int *condition;
    size_t length;
    size_t cap;
} branch_stack;

static void push_condition(int c) {
    if (branch_stack.length == branch_stack.cap) {
        branch_stack.cap += 16;
        branch_stack.condition = 
            realloc(branch_stack.condition, branch_stack.cap * sizeof(int));
    }
    branch_stack.condition[branch_stack.length++] = c;
}

static int peek_condition() {
    return branch_stack.length ? 
        branch_stack.condition[branch_stack.length - 1] : 1;
}

static int pop_condition() {
    if (!branch_stack.length) {
        error("Unmatched #endif directive.");
    }
    return branch_stack.condition[--branch_stack.length];
}

static int expression();

static void preprocess_directive()
{
    struct token t = next_raw_token();

    if (t.token == IF) {
        int val = expression();
        /*printf(" condition value = %d\n", val);*/
        push_condition( peek_condition() ? val : 0 );
    }
    else if (t.token == IDENTIFIER && !strcmp("elif", t.strval)) {
        int val = expression();
        /*printf(" condition value = %d\n", val);*/
        push_condition( !pop_condition() && peek_condition() ? val : 0 );
    }
    else if (t.token == ELSE) {
        push_condition( !pop_condition() && peek_condition() );
    }
    else if (t.token == IDENTIFIER && !strcmp("endif", t.strval)) {
        pop_condition();
    }
    else if (t.token == IDENTIFIER && !strcmp("ifndef", t.strval)) {
        t = next_raw_token();
        push_condition( definition(t) == NULL && peek_condition() );
    }
    else if (t.token == IDENTIFIER && !strcmp("ifdef", t.strval)) {
        t = next_raw_token();
        push_condition( definition(t) != NULL && peek_condition() );
    }
    else if (peek_condition()) {
        if (t.token == IDENTIFIER && !strcmp("define", t.strval)) {
            extern char *line;

            struct token name, subs;
            macro_t *macro;
            toklist_t *params;

            name = next_raw_token();
            if (name.token != IDENTIFIER) {
                error("Definition must be identifier.");
                exit(1);
            }

            params = toklist_init();

            macro = calloc(1, sizeof(macro_t));
            macro->name = name;
            macro->type = OBJECT_LIKE;

            /* Function-like macro iff parenthesis immediately after, access
             * input buffer directly. */
            if (*line == '(') {
                macro->type = FUNCTION_LIKE;
                consume_raw_token('(');
                while (peek_raw_token() != ')') {
                    if (peek_raw_token() != IDENTIFIER) {
                        error("Invalid macro parameter.");
                        exit(1);
                    }
                    toklist_push_back(params, next_raw_token());
                    macro->params++;
                    if (peek_raw_token() != ',') {
                        break;
                    }
                    consume_raw_token(',');
                }
                consume_raw_token(')');
            }

            while (peek_raw_token() != NEWLINE) {
                subs = next_raw_token();
                macro->size++;
                macro->replacement = 
                    realloc(macro->replacement, 
                        macro->size * sizeof(struct macro_subst_t));
                macro->replacement[macro->size - 1].token = subs;
                macro->replacement[macro->size - 1].param = 0;
                if (subs.token == IDENTIFIER) {
                    int i;
                    for (i = 0; i < params->length; ++i) {
                        if (!strcmp(subs.strval, params->elem[i].strval)) {
                            macro->replacement[macro->size - 1].param = i + 1;
                            break;
                        }
                    }
                }
            }

            define_macro(macro);
        }
        else if (t.token == IDENTIFIER && !strcmp("undef", t.strval)) {
            struct token name = next_raw_token();
            undef(name);
        }
        else if (t.token == IDENTIFIER && !strcmp("include", t.strval)) {
            char *path = NULL;
            int angles = 0;
            if (peek_raw_token() == STRING) {
                t = next_raw_token();
                path = (char*) t.strval;
            } else if (peek_raw_token() == '<') {
                consume_raw_token('<');
                angles = 1;
                while (peek_raw_token() != NEWLINE) {
                    if (peek_raw_token() == '>') {
                        break;
                    }
                    t = next_raw_token();
                    path = pastetok(path, t);
                }
                consume_raw_token('>');
            }
            if (!path) {
                error("Invalid include directive.");
                exit(1);
            }
            if (angles) {
                include_system_file(path);
            } else {
                include_file(path);
            }
        }
        else if (t.token == IDENTIFIER && !strcmp("error", t.strval)) {
            extern char *line;

            error("%s", line + 1);
            exit(1);
        }
    } else {
        /* Skip dead code. */
        while (peek_raw_token() != NEWLINE) {
            next_raw_token();
        }
    }
    consume_raw_token(NEWLINE);
}

static void expand_token(struct token t)
{
    macro_t *def = definition(t);

    if (def) {
        int i;
        toklist_t **args = NULL, *res = NULL;

        if (def->type == FUNCTION_LIKE) {

            /* Keep track of the nesting depth of macro arguments. For example
             * MAX( MAX(10, 12), 20 ) should ignore the first comma inside a
             * nested parenthesis. */
            int nesting;

            if (def->params) {
                args = malloc(sizeof(toklist_t *) * def->params);
            }
            consume_raw_token('(');
            for (i = nesting = 0; i < def->params; ++i) {
                args[i] = toklist_init();

                do {
                    t = next_raw_token();
                    if (t.token == ',') {
                        error("Macro expansion does not match definition.");
                        exit(1);
                    }
                    if (t.token == '(') {
                        nesting++;
                    } else if (t.token == ')') {
                        assert(nesting);
                        nesting--;
                    } else if (t.token == NEWLINE) {
                        continue;
                    }
                    toklist_push_back(args[i], t);
                } while (
                    nesting ||
                    (i  < def->params - 1 && peek_raw_token() != ',') ||
                    (i == def->params - 1 && peek_raw_token() != ')')
                );

                if (i < def->params - 1) {
                    consume_raw_token(',');
                }
            }
            consume_raw_token(')');
        }

        res = expand_macro(def, args);

        for (i = 0; i < res->length; ++i) {
            add_token(res->elem[i]);
        }
        for (i = 0; i < def->params; ++i) {
            toklist_destroy(args[i]);
        }
        toklist_destroy(res);
    } else {
        add_token(t);
    }
}

/* Current position in list of tokens ready for parsing, pointing to next token
 * to be returned by next(). */
static size_t cursor;

/* Consume at least one line, up until the final newline or end of file. */
static void preprocess_line()
{
    struct token t;
    size_t remaining;

    remaining = lookahead.length - cursor;
    if (remaining) {
        memmove(lookahead.elem, lookahead.elem + cursor, 
            remaining * sizeof(*lookahead.elem));
    }

    lookahead.length = remaining;
    cursor = 0;

    do {
        t = next_raw_token();

        if (t.token == '#') {
            preprocess_directive();
        } else if (peek_condition()) {
            while (t.token != NEWLINE && t.token != END) {
                expand_token(t);
                t = next_raw_token();
            }
        }
    } while (lookahead.length < K && t.token != END);

    while (lookahead.length < K) {
        assert(t.token == END);
        add_token(t);
    }
}

struct token next() {
    if (cursor + K >= lookahead.length) {
        preprocess_line();
    }
    return lookahead.elem[cursor++];
}

struct token peek() {
    if (!lookahead.length) {
        /* If peek() is the first call made, make sure there is an initial call
         * to populate the lookahead buffer. */
        preprocess_line();
    }
    return lookahead.elem[cursor];
}

struct token peekn(unsigned n) {
    assert(n && n <= K);

    if (!lookahead.length) {
        preprocess_line();
    }
    return lookahead.elem[cursor + n - 1];
}

struct token consume(enum token_type expected) {
    struct token t = next();

    if (t.token != expected) {
        if (isprint(expected)) {
            error("Unexpected token '%s', expected '%c'.", t.strval, expected);
        } else {
            error("Unexpected token '%s'.", t.strval);
        }
        exit(1);
    }

    return t;
}

/* Parse and evaluate token stream corresponding to constant expression.
 * Operators handled in preprocessing expressions are: 
 *  * Integer constants.
 *  * Character constants, which are interpreted as they would be in normal code.
 *  * Arithmetic operators for addition, subtraction, multiplication, division,
 *    bitwise operations, shifts, comparisons, and logical operations (&& and ||).
 *    The latter two obey the usual short-circuiting rules of standard C.
 *  * Macros. All macros in the expression are expanded before actual computation
 *    of the expression's value begins.
 *  * Uses of the defined operator, which lets you check whether macros are 
 *    defined in the middle of an #if.
 *  * Identifiers that are not macros, which are all considered to be the number
 *    zero. This allows you to write #if MACRO instead of #ifdef MACRO, if you 
 *    know that MACRO, when defined, will always have a nonzero value. Function-
 *    like macros used without their function call parentheses are also treated
 *    as zero.
 *
 * Source: http://tigcc.ticalc.org/doc/cpp.html
 */
static int eval_logical_or();
static int eval_logical_and();
static int eval_inclusive_or();
static int eval_exclusive_or();
static int eval_and();
static int eval_equality();
static int eval_relational();
static int eval_shift();
static int eval_additive();
static int eval_multiplicative();
static int eval_unary();
static int eval_primary();
static int expression() {
    int a = eval_logical_or(), b, c;
    if (peek_raw_token() == '?') {
        consume_raw_token('?');
        b = expression();
        consume_raw_token(':');
        c = expression();
        return a ? b : c;
    }
    return a;
}

static int eval_logical_or() {
    int val = eval_logical_and();
    if (peek_raw_token() == LOGICAL_OR) {
        next_raw_token();
        val = eval_logical_or() || val;
    }
    return val;
}

static int eval_logical_and() {
    int val = eval_inclusive_or();
    if (peek_raw_token() == LOGICAL_AND) {
        next_raw_token();
        val = eval_logical_and() && val;
    }
    return val;
}

static int eval_inclusive_or() {
    int val = eval_exclusive_or();
    if (peek_raw_token() == '|') {
        next_raw_token();
        val = eval_inclusive_or() | val;
    }
    return val;
}

static int eval_exclusive_or() {
    int val = eval_and();
    if (peek_raw_token() == '^') {
        next_raw_token();
        val = eval_exclusive_or() ^ val;
    }
    return val;
}

static int eval_and()
{
    int val = eval_equality();
    if (peek_raw_token() == '&') {
        next_raw_token();
        val = eval_and() & val;
    }
    return val;
}

static int eval_equality() {
    int val = eval_relational();
    if (peek_raw_token() == EQ) {
        next_raw_token();
        val = val == eval_equality();
    } else if (peek_raw_token() == NEQ) {
        next_raw_token();
        val = val != eval_equality();
    }
    return val;
}

static int eval_relational() {
    int val = eval_shift(), done = 0;
    do {
        switch (peek_raw_token()) {
            case '<':
                next_raw_token();
                val = val < eval_shift();
                break;
            case '>':
                next_raw_token();
                val = val > eval_shift();
                break;
            case LEQ:
                next_raw_token();
                val = val <= eval_shift();
                break;
            case GEQ:
                next_raw_token();
                val = val >= eval_shift();
                break;
            default:
                done = 1;
                break;
        }
    } while (!done);
    return val;
}

static int eval_shift() {
    int val = eval_additive(), done = 0;
    do {
        switch (peek_raw_token()) {
            case LSHIFT:
                next_raw_token();
                val = val << eval_additive();
                break;
            case RSHIFT:
                next_raw_token();
                val = val >> eval_additive();
                break;
            default:
                done = 1;
                break;
        }
    } while (!done);
    return val;
}

static int eval_additive() {
    int val = eval_multiplicative(), done = 0;
    do {
        switch (peek_raw_token()) {
            case '+':
                next_raw_token();
                val = val + eval_multiplicative();
                break;
            case '-':
                next_raw_token();
                val = val - eval_multiplicative();
                break;
            default:
                done = 1;
                break;
        }
    } while (!done);
    return val;
}

static int eval_multiplicative() {
    int val = eval_unary(), done = 0;
    do {
        switch (peek_raw_token()) {
            case '*':
                next_raw_token();
                val = val * eval_unary();
                break;
            case '/':
                next_raw_token();
                val = val / eval_unary();
                break;
            case '%':
                next_raw_token();
                val = val % eval_unary();
                break;
            default:
                done = 1;
                break;
        }
    } while (!done);
    return val;
}

static int eval_unary() {
    switch (peek_raw_token()) {
        case '+':
            next_raw_token();
            return eval_unary();
        case '-':
            next_raw_token();
            return - eval_unary();
        case '~':
            next_raw_token();
            return ~ eval_unary();
        case '!':
            next_raw_token();
            return ! eval_unary();
        default:
            return eval_primary();
    }
}

static int eval_primary() {
    int value;
    macro_t *def;
    struct token t;

    t = next_raw_token();
    switch (t.token) {
        case INTEGER_CONSTANT:
            return t.intval;
        case IDENTIFIER:
            if (!strcmp("defined", t.strval)) {
                t = next_raw_token();
                if (t.token == '(') {
                    t = next_raw_token();
                    consume_raw_token(')');
                }
                if (t.token == IDENTIFIER) {
                    def = definition(t);
                    return def != NULL;
                }
                error("Invalid defined check.");
                exit(1);
            } else {
                def = definition(t);
                return def ? def->replacement[0].token.intval : 0;
            }
            return 0;
        case '(':
            value = expression();
            consume_raw_token(')');
            return value;
        default:
            error("Invalid primary expression.");
            exit(1);
            return 0;
    }
}
