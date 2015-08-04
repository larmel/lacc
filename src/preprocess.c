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

/* Add consecutive space tokens, returning the first token that is of another
 * type.
 */
static struct token get_next(toklist_t *list)
{
    struct token t = get_preprocessing_token();
    while (t.token == SPACE) {
        toklist_push_back(list, t);
        t = get_preprocessing_token();
    }
    return t;
}

/* Skip through whitespace and add token of expected type. Whitespace is also
 * added.
 */
static void expect_next(toklist_t *list, enum token_type type)
{
    struct token t = get_next(list);
    if (t.token != type) {
        error("Expected token '%c', but got '%s'.", (char) type, t.strval);
        exit(1);
    }
    toklist_push_back(list, t);
}

/* Keep track of the nesting depth of macro arguments. For example MAX( MAX(10,
 * 12), 20 ) should complete on the last parenthesis, which makes the expression
 * balanced. Read lines until full macro invocation is included.
 */
static void read_macro_invocation(toklist_t *list, macro_t *macro)
{
    int nesting = 1;
    assert(macro->type == FUNCTION_LIKE);

    expect_next(list, '(');
    while (nesting) {
        struct token t = get_next(list);
        if (t.token == '(') {
            nesting++;
        }
        if (t.token == ')') {
            assert(nesting);
            nesting--;
        }
        if (t.token == NEWLINE) {
            /* This is the only scenario where reading a line is not enough.
             * Macro invocations can span lines, and we want to have
             * everything in the same token list. */
            continue;
        }
        if (t.token == END) {
            break;
        }
        toklist_push_back(list, t);
    }
    if (nesting) {
        error("Unbalanced invocation of macro '%s'.", macro->name.strval);
        exit(1);
    }
}

/* Replace defined name and defined (name) with 0 or 1 constants.
 */
static void read_defined_operator(toklist_t *list)
{
    int is_parens = 0;
    struct token t = get_next(list);

    if (t.token == '(') {
        t = get_next(list);
        is_parens = 1;
    }
    if (t.token != IDENTIFIER) {
        error("Expected identifier in 'defined' clause, but got '%s'",
            t.strval);
        exit(1);
    }
    if (definition(t)) {
        t.intval = 1;
        t.strval = "1";
    } else {
        t.intval = 0;
        t.strval = "0";
    }
    t.token = INTEGER_CONSTANT;
    toklist_push_back(list, t);
    if (is_parens) {
        t = get_next(list);
        if (t.token != ')') {
            error("Expected '(' but got '%s'.", t.strval);
            exit(1);
        }
    }
}

/* Read tokens until reaching newline or eof. If initial token is '#', stop on
 * newline. Otherwise make sure macro invocations spanning multiple lines are
 * joined, and replace 'defined' directives with constants. Return newline or $.
 */
static struct token read_complete_line(toklist_t *list, struct token t)
{
    int is_directive = (t.token == '#');
    macro_t *def;

    while (t.token != NEWLINE && t.token != END) {
        if (t.token == IDENTIFIER) {
            if (!strcmp("defined", t.strval) && is_directive) {
                read_defined_operator(list);
            } else if ((def = definition(t)) && def->type == FUNCTION_LIKE) {
                toklist_push_back(list, t);
                read_macro_invocation(list, def);
            } else {
                toklist_push_back(list, t);
            }
        } else {
            toklist_push_back(list, t);
        }
        t = get_preprocessing_token();
    }
    return t;
}

/* A token list, but with additional pointer to current element.
 */
struct token_stream
{
    unsigned next;
    toklist_t *list;
};

static void ts_skip_ws(struct token_stream *stream)
{
    while (stream->next < stream->list->length) {
        if (stream->list->elem[stream->next].token == SPACE) {
            stream->next++;
        } else break;
    }
}

/* Progress pointer, returning the next non-whitespace element.
 */
static struct token ts_next(struct token_stream *stream)
{
    ts_skip_ws(stream);
    assert(stream->next < stream->list->length);
    return stream->list->elem[stream->next++];
}

/* Move past next token, failing with an error if it does not match the expected
 * type.
 */
static void ts_consume(struct token_stream *stream, enum token_type type)
{
    struct token t = ts_next(stream);
    if (t.token != type) {
        error("Unexpected preprocessing token '%s', expected '%c'.",
            t.strval, type);
        exit(1);
    }
}

/* Return next element or EOF, without changing pointer.
 */
static enum token_type ts_peek(struct token_stream *stream)
{
    ts_skip_ws(stream);
    if (stream->next == stream->list->length) {
        return END;
    }
    return stream->list->elem[stream->next].token;
}

static int expression(struct token_stream *stream);

static int eval_primary(struct token_stream *stream)
{
    int value = 0;
    struct token t = ts_next(stream);

    switch (t.token) {
    case INTEGER_CONSTANT:
        value = t.intval;
        break;
    case IDENTIFIER:
        /* Macro expansions should already have been done. Stray identifiers are
         * interpreted as zero constants. */
        assert(!definition(t));
        break;
    case '(':
        value = expression(stream);
        ts_consume(stream, ')');
        break;
    default:
        error("Invalid primary expression.");
        break;
    }
    return value;
}

static int eval_unary(struct token_stream *stream)
{
    switch (ts_peek(stream)) {
    case '+':
        ts_next(stream);
        return eval_unary(stream);
    case '-':
        ts_next(stream);
        return - eval_unary(stream);
    case '~':
        ts_next(stream);
        return ~ eval_unary(stream);
    case '!':
        ts_next(stream);
        return ! eval_unary(stream);
    default:
        return eval_primary(stream);
    }
}

static int eval_multiplicative(struct token_stream *stream)
{
    int val = eval_unary(stream), done = 0;
    do {
        switch (ts_peek(stream)) {
        case '*':
            ts_next(stream);
            val = val * eval_unary(stream);
            break;
        case '/':
            ts_next(stream);
            val = val / eval_unary(stream);
            break;
        case '%':
            ts_next(stream);
            val = val % eval_unary(stream);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    return val;
}

static int eval_additive(struct token_stream *stream)
{
    int val = eval_multiplicative(stream), done = 0;
    do {
        switch (ts_peek(stream)) {
        case '+':
            ts_next(stream);
            val = val + eval_multiplicative(stream);
            break;
        case '-':
            ts_next(stream);
            val = val - eval_multiplicative(stream);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    return val;
}

static int eval_shift(struct token_stream *stream)
{
    int val = eval_additive(stream), done = 0;
    do {
        switch (ts_peek(stream)) {
        case LSHIFT:
            ts_next(stream);
            val = val << eval_additive(stream);
            break;
        case RSHIFT:
            ts_next(stream);
            val = val >> eval_additive(stream);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    return val;
}

static int eval_relational(struct token_stream *stream)
{
    int val = eval_shift(stream), done = 0;
    do {
        switch (ts_peek(stream)) {
        case '<':
            ts_next(stream);
            val = val < eval_shift(stream);
            break;
        case '>':
            ts_next(stream);
            val = val > eval_shift(stream);
            break;
        case LEQ:
            ts_next(stream);
            val = val <= eval_shift(stream);
            break;
        case GEQ:
            ts_next(stream);
            val = val >= eval_shift(stream);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    return val;
}

static int eval_equality(struct token_stream *stream)
{
    int val = eval_relational(stream);
    if (ts_peek(stream) == EQ) {
        ts_next(stream);
        val = val == eval_equality(stream);
    } else if (ts_peek(stream) == NEQ) {
        ts_next(stream);
        val = val != eval_equality(stream);
    }
    return val;
}

static int eval_and(struct token_stream *stream)
{
    int val = eval_equality(stream);
    if (ts_peek(stream) == '&') {
        ts_next(stream);
        val = eval_and(stream) & val;
    }
    return val;
}

static int eval_exclusive_or(struct token_stream *stream)
{
    int val = eval_and(stream);
    if (ts_peek(stream) == '^') {
        ts_next(stream);
        val = eval_exclusive_or(stream) ^ val;
    }
    return val;
}

static int eval_inclusive_or(struct token_stream *stream)
{
    int val = eval_exclusive_or(stream);
    if (ts_peek(stream) == '|') {
        ts_next(stream);
        val = eval_inclusive_or(stream) | val;
    }
    return val;
}

static int eval_logical_and(struct token_stream *stream)
{
    int val = eval_inclusive_or(stream);
    if (ts_peek(stream) == LOGICAL_AND) {
        ts_next(stream);
        val = eval_logical_and(stream) && val;
    }
    return val;
}

static int eval_logical_or(struct token_stream *stream)
{
    int val = eval_logical_and(stream);
    if (ts_peek(stream) == LOGICAL_OR) {
        ts_next(stream);
        val = eval_logical_or(stream) || val;
    }
    return val;
}

static int expression(struct token_stream *stream)
{
    int a = eval_logical_or(stream), b, c;
    if (ts_peek(stream) == '?') {
        ts_consume(stream, '?');
        b = expression(stream);
        ts_consume(stream, ':');
        c = expression(stream);
        return a ? b : c;
    }
    return a;
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

static void preprocess_directive(toklist_t *list)
{
    struct token t;
    struct token_stream stream;

    stream.list = list;
    stream.next = 0;

    t = ts_next(&stream);
    assert(t.token == '#');
    t = ts_next(&stream);
    if (t.token == IF || (t.token == IDENTIFIER && !strcmp("elif", t.strval))) {
        /* Perform macro expansion only for if and elif directives, before doing
         * the expression parsing. Next element pointer should stay the same. */
        stream.list = expand(stream.list);
    }

    if (t.token == IF) {
        int val = expression(&stream);
        push_condition(peek_condition() ? val : 0);
    } else if (t.token == IDENTIFIER && !strcmp("elif", t.strval)) {
        int val = expression(&stream);
        push_condition(!pop_condition() && peek_condition() ? val : 0);
    } else if (t.token == ELSE) {
        push_condition(!pop_condition() && peek_condition());
    } else if (t.token == IDENTIFIER && !strcmp("endif", t.strval)) {
        pop_condition();
    } else if (t.token == IDENTIFIER && !strcmp("ifndef", t.strval)) {
        t = ts_next(&stream);
        push_condition(!definition(t) && peek_condition());
    } else if (t.token == IDENTIFIER && !strcmp("ifdef", t.strval)) {
        t = ts_next(&stream);
        push_condition(definition(t) && peek_condition());
    } else if (peek_condition()) {
        if (t.token == IDENTIFIER && !strcmp("define", t.strval)) {
            macro_t *macro = calloc(1, sizeof(macro_t));
            toklist_t *params = toklist_init();

            macro->name = ts_next(&stream);
            macro->type = OBJECT_LIKE;
            if (macro->name.token != IDENTIFIER) {
                error("Definition must be identifier.");
                exit(1);
            }

            /* Function-like macro iff parenthesis immediately after, access
             * input stream directly. */
            if (stream.list->elem[stream.next].token == '(') {
                macro->type = FUNCTION_LIKE;
                ts_consume(&stream, '(');
                while (ts_peek(&stream) != ')') {
                    if (ts_peek(&stream) != IDENTIFIER) {
                        error("Invalid macro parameter.");
                        exit(1);
                    }
                    toklist_push_back(params, ts_next(&stream));
                    macro->params++;
                    if (ts_peek(&stream) != ',') {
                        break;
                    }
                    ts_consume(&stream, ',');
                }
                ts_consume(&stream, ')');
            }
            while (ts_peek(&stream) != NEWLINE) {
                struct token subs = ts_next(&stream);
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
        } else if (t.token == IDENTIFIER && !strcmp("undef", t.strval)) {
            struct token name = ts_next(&stream);
            undef(name);
        } else if (t.token == IDENTIFIER && !strcmp("include", t.strval)) {
            char *path = NULL;
            int angles = 0;
            if (ts_peek(&stream) == STRING) {
                t = ts_next(&stream);
                path = (char*) t.strval;
            } else if (ts_peek(&stream) == '<') {
                ts_consume(&stream, '<');
                angles = 1;
                while (ts_peek(&stream) != NEWLINE) {
                    if (ts_peek(&stream) == '>') {
                        break;
                    }
                    t = ts_next(&stream);
                    path = pastetok(path, t);
                }
                ts_consume(&stream, '>');
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
        } else if (t.token == IDENTIFIER && !strcmp("error", t.strval)) {
            t = toklist_to_string(stream.list);
            error("%s", t.strval);
            exit(1);
        }
    } else {
        /* Skip dead code. */
        while (ts_peek(&stream) != NEWLINE) {
            ts_next(&stream);
        }
    }
    ts_consume(&stream, NEWLINE);
}

/* Toggle for producing preprocessed output (-E).
 */
static int preserve_whitespace;

/* Buffer of preprocessed tokens, ready to be consumed by the parser. Configured
 * to hold at least K tokens, enabling LL(K) parsing.
 */
static struct toklist lookahead;

/* For the K&R grammar, it is sufficient to have K = 2.
 */
static const int K = 2;

static void add_token(struct token t)
{
    extern int VERBOSE;

    toklist_push_back(&lookahead, t);

    if (VERBOSE) {
        debug_output_token(t);
    }
}

/* Current position in lookahead buffer, pointing to next token to be returned
 * by next().
 */
static size_t cursor;

/* Consume at least one line, up until the final newline or end of file.
 */
static void preprocess_line(void)
{
    struct token t;
    size_t remaining;

    /* Update lookahead buffer. */
    remaining = lookahead.length - cursor;
    if (remaining) {
        memmove(lookahead.elem, lookahead.elem + cursor, 
            remaining * sizeof(*lookahead.elem));
    }
    lookahead.length = remaining;
    cursor = 0;

    /* Consume and preprocess lines until lookahead is met, or end of input. */
    do {
        toklist_t *tokens = toklist_init();

        t = get_next(tokens);
        if (t.token == '#') {
            t = read_complete_line(tokens, t);
            if (t.token == NEWLINE) {
                toklist_push_back(tokens, t);
            }
            preprocess_directive(tokens);
        } else if (peek_condition()) {
            int i;

            t = read_complete_line(tokens, t);
            if (preserve_whitespace && t.token == NEWLINE) {
                toklist_push_back(tokens, t);
            }
            tokens = expand(tokens);
            for (i = 0; i < tokens->length; ++i) {
                t = tokens->elem[i];
                if ((t.token != NEWLINE && t.token != SPACE) ||
                    preserve_whitespace)
                {
                    add_token(t);
                }
            }
        }
        toklist_destroy(tokens);
    } while (lookahead.length < K && t.token != END);

    /* Fill remainder of lookahead buffer with $. */
    while (lookahead.length < K) {
        assert(t.token == END);
        add_token(t);
    }
}

struct token next()
{
    if (cursor + K >= lookahead.length) {
        preprocess_line();
    }
    return lookahead.elem[cursor++];
}

struct token peek()
{
    if (!lookahead.length) {
        /* If peek() is the first call made, make sure there is an initial call
         * to populate the lookahead buffer. */
        preprocess_line();
    }
    return lookahead.elem[cursor];
}

struct token peekn(unsigned n)
{
    assert(n && n <= K);

    if (!lookahead.length) {
        preprocess_line();
    }
    return lookahead.elem[cursor + n - 1];
}

struct token consume(enum token_type expected)
{
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

void preprocess(FILE *output)
{
    struct token t;

    preserve_whitespace = 1;
    t = next();
    while (t.token != END) {
        switch (t.token) {
        case INTEGER_CONSTANT:
            fprintf(output, "%ld", t.intval);
            break;
        case STRING:
            fprintf(output, "\"%s\"", t.strval);
            break;
        default:
            fprintf(output, "%s", t.strval);
            break;
        }
        t = next();
    }
}
