#include "error.h"
#include "input.h"
#include "preprocess.h"
#include "util/map.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Hold current clean line to be tokenized. */
static char *tok;

typedef struct {
    token_t name;
    token_t *subst;
    size_t size;
} macro_t;

/* Use one lookahead for preprocessing token. */
static token_t prep_token_peek;
static int has_prep_token_peek;

static void debug_output_token(token_t t)
{
    switch (t.token) {
        case IDENTIFIER:
        case STRING:
            printf("  token( %s, %d )\n", t.strval, (int)t.token);
            break;
        case INTEGER_CONSTANT:
            printf("  token( %ld )\n", t.intval);
            break;
        default:
            if (isprint(t.token)) {
                printf("  token( %c, %d )\n", t.token, (int)t.token);
            } else {
                printf("  token( %s, %d )\n", t.strval, (int)t.token);
            }
            break;
    }
}

static token_t next_raw_token()
{
    extern token_t get_token(char *, char **);
    
    token_t r;
    char *end;

    if (has_prep_token_peek) {
        has_prep_token_peek = 0;
        return prep_token_peek;
    }

    r = get_token(tok, &end);
    tok = end;
    /*debug_output_token(r); */
    return r;
}

static enum token peek_raw_token()
{
    if (has_prep_token_peek) {
        return prep_token_peek.token;
    }

    prep_token_peek = next_raw_token();
    has_prep_token_peek = 1;

    /*debug_output_token(prep_token_peek); */

    return prep_token_peek.token;
}

static void consume_raw_token(enum token t)
{
    token_t read = next_raw_token();

    if (read.token != t) {
        error("Unexpected preprocessing token.");
        printf("  -> Token was:");
        debug_output_token(read);
        if (isprint((int) t)) {
            printf("  -> Expected %c\n", (char) t);
        }
        exit(1);
    }
}

/* Store list of preprocessed tokens. Read next from list on call to token(),
 * peek() or consume(), and start over after call to get_preprocessed_tokens().
 */
static struct {
    token_t *tokens;
    size_t length;
    size_t cap;
} toklist;

static size_t add_token(token_t t)
{
    toklist.length++;
    if (toklist.length > toklist.cap) {
        toklist.cap += 64;
        toklist.tokens = realloc(toklist.tokens, toklist.cap * sizeof(token_t));
    }
    toklist.tokens[toklist.length - 1] = t;

    /*debug_output_token(t);*/

    return toklist.length;
}

/* 
 * Push and pop branch conditions for #if, #elif and #endif.
 */
static struct {
    int *condition;
    size_t length;
    size_t cap;
} branch_stack;

static void push_condition(int c) {
    if (branch_stack.length == branch_stack.cap) {
        branch_stack.cap += 16;
        branch_stack.condition = realloc(branch_stack.condition, branch_stack.cap * sizeof(int));
    }
    branch_stack.condition[branch_stack.length++] = c;
}

static int peek_condition() {
    return branch_stack.length ? branch_stack.condition[branch_stack.length - 1] : 1;
}

static int pop_condition() {
    if (!branch_stack.length) {
        error("Unmatched #endif directive.");
    }
    return branch_stack.condition[--branch_stack.length];
}

/* 
 * Macro definitions.
 */
static map_t definitions;

static void define_macro(macro_t *macro)
{
    map_insert(&definitions, macro->name.strval, (void *) macro);
}

static void define(token_t name, token_t subst)
{
    macro_t *macro;

    assert(name.strval);

    macro = map_lookup(&definitions, name.strval);
    if (!macro) {
        macro_t *p = malloc(sizeof(macro_t));
        p->name = name;
        p->subst = malloc(1 * sizeof(token_t));
        p->subst[0] = subst;
        p->size = 1;
        map_insert(&definitions, name.strval, (void *) p);
    }
}

static void undef(token_t name)
{
    assert(name.strval);

    /* No-op if name is not a macro. */
    map_remove(&definitions, name.strval);
}

static macro_t *definition(token_t name)
{
    if (!name.strval) {
        return NULL;
    }
    return map_lookup(&definitions, name.strval);
}

void register_builtin_definitions()
{
    token_t 
        name = { IDENTIFIER, NULL, 0 },
        valu = { INTEGER_CONSTANT, NULL, 0 };

    name.strval = "__STDC_VERSION__";
    valu.intval = 199409L;
    define( name, valu );

    name.strval = "__STDC__";
    valu.intval = 1;
    define( name, valu );

    name.strval = "__STDC_HOSTED__";
    valu.intval = 1;
    define( name, valu );
}

/* Append token string representation at the end of provided buffer. If NULL is
 * provided, a new buffer is allocated that must be free'd by caller. */
static char *pastetok(char *buf, token_t t) {
    size_t len;

    if (!buf) {
        buf = calloc(16, sizeof(char));
        len = 0;
    } else {
        len = strlen(buf);
        if (t.strval) {
            buf = realloc(buf, len + strlen(t.strval) + 1);
        } else {
            buf = realloc(buf, len + 32);
        }
    }

    if (t.strval) {
        strcat(buf, t.strval);
    } else if (t.intval) {
        sprintf(buf + len, "%ld", t.intval);
    } else {
        assert(isprint(t.token));
        sprintf(buf + len, "%c", t.token);
    }

    return buf;
}

static int expression();

static void preprocess_directive()
{
    token_t t = next_raw_token();

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
            token_t name, subs;
            macro_t *macro;

            name = next_raw_token();
            if (name.token != IDENTIFIER) {
                error("Definition must be identifier.");
                exit(1);
            }

            macro = calloc(1, sizeof(macro_t));
            macro->name = name;

            while (peek_raw_token() != END) {
                subs = next_raw_token();
                macro->size++;
                macro->subst = realloc(macro->subst, macro->size * sizeof(token_t));
                macro->subst[macro->size - 1] = subs;
            }

            define_macro(macro);
        }
        else if (t.token == IDENTIFIER && !strcmp("undef", t.strval)) {
            token_t name = next_raw_token();
            undef(name);
        }
        else if (t.token == IDENTIFIER && !strcmp("include", t.strval)) {
            char *path = NULL;
            if (peek_raw_token() == STRING) {
                t = next_raw_token();
                path = (char*) t.strval;
            } else if (peek_raw_token() == '<') {
                consume_raw_token('<');
                while (peek_raw_token() != END) {
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
            include_file(path);
        }
        else if (t.token == IDENTIFIER && !strcmp("error", t.strval)) {
            error("%s", tok + 1);
            exit(1);
        }
    } else {
        /* Skip the rest. */
        return;
    }
    consume_raw_token(END);
}

/* Filter token stream and perform preprocessor tasks such as macro substitution,
 * file inclusion etc. One line is processed at a time, putting the resulting
 * parse-ready tokens in token_list. Iterate until the current line yields at
 * least one token for parsing. */
static int preprocess_line()
{
    token_t t;

    do {
        /*assert(tok == NULL || *tok == '\0');*/

        /* Get a new clean line, with comments and line continuations removed. */
        if (getprepline(&tok) == -1)
            return 0;

        /* Reset list of preprocessed tokens. */
        toklist.length = 0;
        t = next_raw_token();

        if (t.token == '#') {
            preprocess_directive();
            if (*tok != '\0') {
                /*puts("skipped tokens"); */
            }
        } else if (peek_condition()) {
            while (t.token != END) {
                macro_t *def = definition(t);
                if (def) {
                    int i;
                    for (i = 0; i < def->size; ++i) {
                        add_token(def->subst[i]);
                    }
                } else {
                    add_token(t);
                }
                t = next_raw_token();
            }
        }

        has_prep_token_peek = 0;
    } while (!toklist.length);

    return toklist.length;
}

/* Current position in list of tokens ready for parsing. */
static size_t current = -1;

/* 
 * External interface.
 */

long intval;
const char *strval;

/* Move current pointer one step forward, returning the next token. */
enum token token() {
    if (current + 1 == toklist.length) {
        if (!preprocess_line()) {
            return END;
        }
        current = -1;
    }
    current++;

    strval = toklist.tokens[current].strval;
    intval = toklist.tokens[current].intval;
    return toklist.tokens[current].token;
}

enum token peek() {
    if (current + 1 == toklist.length) {
        if (!preprocess_line()) {
            return END;
        }
        current = -1;
    }

    strval = toklist.tokens[current + 1].strval;
    intval = toklist.tokens[current + 1].intval;
    return toklist.tokens[current + 1].token;
}

void consume(enum token expected) {
    enum token t = token();
    if (t != expected) {
        if (isprint(t)) {
            if (isprint(expected))
                error("Unexpected token `%c`, expected `%c`.", t, expected);
            else
                error("Unexpected token `%c`.", t);
        } else {
            if (isprint(expected))
                error("Unexpected token `%s`, expected `%c`.", strval, expected);
            else
                error("Unexpected token `%s`.", strval);
        }
        exit(1);
    }
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
    token_t t;

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
                return def ? def->subst[0].intval : 0;
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
