#include "directive.h"
#include "input.h"
#include "macro.h"
#include "preprocess.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/context.h>
#include <lacc/deque.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Lookahead kept in buffer of preprocessed tokens. For the K&R grammar,
 * it is sufficient to have two lookahead to parse.
 */
#define K 2

/* Buffer of preprocessed tokens, ready to be consumed by the parser.
 * Configured to hold at least K tokens, enabling LL(K) parsing.
 */
static deque_of(struct token) lookahead;

/* Toggle for producing preprocessed output (-E).
 */
static int output_preprocessed;

/* Line currently being tokenized.
 */
static char *line_buffer;

static void cleanup(void)
{
    deque_destroy(&lookahead);
}

static void ensure_initialized(void)
{
    static int done;

    if (!done) {
        atexit(cleanup);
        done = 1;
    }
}

static struct token get_token(void)
{
    struct token r;
    char *endptr;

    if (!line_buffer && (line_buffer = getprepline()) == NULL) {
        r = basic_token[END];
    } else {
        r = tokenize(line_buffer, &endptr);
        line_buffer = endptr;
        if (r.token == END) {
            /* Newlines are removed by getprepline, and never present in
             * the input data. Instead intercept end of string, which
             * represents end of line. */
            line_buffer = NULL;
            r = basic_token[NEWLINE];
        }
    }

    return r;
}

/* Keep track of the nesting depth of macro arguments. For example;
 * MAX( MAX(10, 12), 20 ) should complete on the last parenthesis, which
 * makes the expression balanced. Read lines until full macro invocation
 * is included.
 */
static void read_macro_invocation(TokenArray *line, const struct macro *macro)
{
    int nesting;
    struct token t;
    assert(macro->type == FUNCTION_LIKE);

    t = get_token();
    array_push_back(line, t);
    if (t.token != '(')
        /* Only expand function-like macros if they appear as function
         * invocations, beginning with an open paranthesis. */
        return;

    nesting = 1;
    while (nesting) {
        t = get_token();
        if (t.token == '(') {
            nesting++;
        }
        if (t.token == ')') {
            assert(nesting);
            nesting--;
        }
        if (t.token == NEWLINE) {
            /* This is the only scenario where reading a line is not
             * enough. Macro invocations can span lines, and we want to
             * have everything in the same token list. */
            continue;
        }
        assert(t.token != END);
        array_push_back(line, t);
    }
    if (nesting) {
        error("Unbalanced invocation of macro '%s'.", str_raw(macro->name));
        exit(1);
    }
}

/* Replace 'defined name' and 'defined (name)' with 0 or 1 constants.
 */
static void read_defined_operator(TokenArray *line)
{
    int is_parens = 0;
    char *endptr;
    struct token t = get_token();

    if (t.token == '(') {
        t = get_token();
        is_parens = 1;
    }

    if (t.token != IDENTIFIER) {
        error("Expected identifier in 'defined' clause, but got '%s'",
            str_raw(t.d.string));
        exit(1);
    }

    if (definition(tokstr(t)))
        t = tokenize("1", &endptr);
    else
        t = tokenize("0", &endptr);

    array_push_back(line, t);
    if (is_parens) {
        t = get_token();
        if (t.token != ')') {
            error("Expected ')' to close 'defined' clause.");
            exit(1);
        }
    }
}

/* Read tokens until reaching end of line. If initial token is '#', stop
 * on first newline. Otherwise make sure macro invocations spanning mul-
 * tiple lines are joined, and replace 'defined' directives with 0 or 1.
 *
 * Returns a buffer containing all necessary tokens to preprocess a
 * line. Always ends with a newline (\n) token, but never contains any
 * newlines in the array itself.
 */
static void read_complete_line(TokenArray *line, struct token t, int directive)
{
    int expandable = 1;
    const struct macro *def;

    if (directive) {
        array_push_back(line, t);
        expandable = (t.token == IF) || !tok_cmp(t, ident__elif);
        t = get_token();
    }

    while (t.token != NEWLINE) {
        assert(t.token != END);
        if (t.token == IDENTIFIER) {
            if (!tok_cmp(t, ident__defined) && directive && expandable) {
                read_defined_operator(line);
            } else if ((def = definition(tokstr(t)))
                && def->type == FUNCTION_LIKE
                && expandable)
            {
                array_push_back(line, t);
                read_macro_invocation(line, def);
            } else {
                array_push_back(line, t);
            }
        } else {
            array_push_back(line, t);
        }
        t = get_token();
    }

    assert(t.token == NEWLINE);
    array_push_back(line, t);
}

static void add_to_lookahead(struct token t)
{
    String s;
    unsigned len = deque_len(&lookahead);
    struct token prev;
    int added = 0;

    /* Combine adjacent string literals. This step is done after
     * preprocessing and macro expansion; logic in preprocess_line will
     * guarantee that we keep preprocessing lines and filling up the
     * lookahead buffer for as long as there can be continuations. */
    if (t.token == STRING && len) {
        prev = deque_get(&lookahead, len - 1);
        if (prev.token == STRING) {
            deque_get(&lookahead, len - 1) = pastetok(prev, t);
            added = 1;
        }
    }

    /* Convert preprocessing numbers to actual numeric tokens, unless
     * doing only preprocessing. */
    if (!output_preprocessed && t.token == PREP_NUMBER) {
        t = convert_preprocessing_number(t);
    }

    if (!added) {
        deque_push_back(&lookahead, t);
    }

    if (context.verbose) {
        s = tokstr(t);
        verbose("   token( %s )", str_raw(s));
    }
}

/* Consume at least one line, up until the final newline or end of file.
 * Fill up lookahead buffer to hold at least K tokens. In case of end of
 * input, put END tokens in remaining slots.
 */
static void preprocess_line(void)
{
    static TokenArray line;

    unsigned i;
    struct token t, u;

    ensure_initialized();
    do {
        t = get_token();
        if (t.token == END) {
            array_clear(&line);
            break;
        }

        line.length = 0;
        if (t.token == '#') {
            t = get_token();
            if (in_active_block() || (
                t.token == IF ||
                !tok_cmp(t, ident__ifdef) ||
                !tok_cmp(t, ident__ifndef) ||
                !tok_cmp(t, ident__elif) ||
                !tok_cmp(t, ident__endif) ||
                t.token == ELSE))
            {
                read_complete_line(&line, t, 1);
                preprocess_directive(&line);
            } else {
                line_buffer = NULL;
            }
        } else {
            assert(in_active_block());
            read_complete_line(&line, t, 0);
            expand(&line);
            for (i = 0; i < array_len(&line); ++i) {
                u = array_get(&line, i);
                if (u.token != NEWLINE || output_preprocessed) {
                    if (u.token != NEWLINE)
                        t = u;
                    add_to_lookahead(u);
                }
            }
        }
    } while (deque_len(&lookahead) < K || t.token == STRING);

    while (deque_len(&lookahead) < K) {
        add_to_lookahead(basic_token[END]);
    }
}

struct token next(void)
{
    if (deque_len(&lookahead) <= K) {
        preprocess_line();
    }

    return deque_pop_front(&lookahead);
}

struct token peek(void)
{
    return peekn(1);
}

struct token peekn(unsigned n)
{
    assert(n && n <= K);
    if (!deque_len(&lookahead)) {
        /* If peek() is the first call made, make sure there is an
         * initial call to populate the lookahead buffer. */
        preprocess_line();
    }

    return deque_get(&lookahead, n - 1);
}

struct token consume(enum token_type type)
{
    String s;
    struct token t = next();

    if (t.token != type) {
        s = tokstr(t);
        switch (type) {
        case IDENTIFIER:
        case NUMBER:
        case STRING:
            error("Unexpected token '%s', expected %s.",
                str_raw(s),
                (type == IDENTIFIER) ? "identifier" :
                (type == NUMBER) ? "number" : "string");
            break;
        default:
            error("Unexpected token '%s', expected '%s'.",
                str_raw(s), str_raw(basic_token[type].d.string));
            break;
        }
        exit(1);
    }

    return t;
}

void preprocess(FILE *output)
{
    struct token t;
    String s;

    output_preprocessed = 1;
    while ((t = next()).token != END) {
        if (t.leading_whitespace) {
            fprintf(output, "%*s", t.leading_whitespace, " ");
        }
        if (t.token == STRING) {
            fprintstr(output, t.d.string); 
        } else {
            s = tokstr(t);
            fprintf(output, "%s", str_raw(s));
        }
    }
}
