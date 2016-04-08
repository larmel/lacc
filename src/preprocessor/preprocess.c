#include "directive.h"
#include "input.h"
#include "macro.h"
#include "preprocess.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/array.h>
#include <lacc/cli.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Toggle for producing preprocessed output (-E).
 */
static int preserve_whitespace;

/* Buffer of preprocessed tokens, ready to be consumed by the parser.
 * Configured to hold at least K tokens, enabling LL(K) parsing.
 *
 * For the K&R grammar, it is sufficient to have K = 2.
 *
 * Cursor points to current position in lookahead buffer, the token to
 * be returned by next().
 */
static TokenArray lookahead;
static unsigned cursor;
static const int K = 2;

/* Line currently being tokenized.
 */
static char *line_buffer;

static void cleanup(void)
{
    array_clear(&lookahead);
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
static void read_macro_invocation(TokenArray *line, const struct macro *macro) {
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
        if (t.token == END) {
            break;
        }
        array_push_back(line, t);
    }
    if (nesting) {
        error("Unbalanced invocation of macro '%s'.", macro->name.d.string.str);
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
            t.d.string.str);
        exit(1);
    }

    if (definition(t))
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

/* Read tokens until reaching newline or eof. If initial token is '#',
 * stop on newline. Otherwise make sure macro invocations spanning
 * multiple lines are joined, and replace 'defined' directives with
 * constants.
 *
 * Returns a buffer containing all necessary tokens to preprocess a
 * line.
 */
static struct token *read_complete_line(struct token t, int is_directive)
{
    TokenArray line = {0};
    const struct macro *def;
    int is_expandable = 1;

    if (is_directive) {
        array_push_back(&line, t);
        is_expandable = (t.token == IF) || !tok_cmp(t, ident__elif);
        t = get_token();
    }

    while (t.token != NEWLINE && t.token != END) {
        if (t.token == IDENTIFIER) {
            if (!tok_cmp(t, ident__defined) && is_directive && is_expandable) {
                read_defined_operator(&line);
            } else if (
                (def = definition(t)) && def->type == FUNCTION_LIKE &&
                is_expandable)
            {
                array_push_back(&line, t);
                read_macro_invocation(&line, def);
            } else {
                array_push_back(&line, t);
            }
        } else {
            array_push_back(&line, t);
        }
        t = get_token();
    }

    if (preserve_whitespace && t.token == NEWLINE)
        array_push_back(&line, t);

    array_push_back(&line, basic_token[END]);
    return line.data;
}

static void add_to_lookahead(struct token t)
{
    unsigned i = array_len(&lookahead);
    struct token prev;
    int added = 0;

    /* Combine adjacent string literals. This step is done after
     * preprocessing and macro expansion; logic in preprocess_line will
     * guarantee that we keep preprocessing lines and filling up the
     * lookahead buffer for as long as there can be continuations. */
    if (t.token == STRING && i--) {
        prev = array_get(&lookahead, i);
        if (prev.token == STRING) {
            array_get(&lookahead, i) = pastetok(prev, t);
            added = 1;
        }
    }

    if (!added) {
        array_push_back(&lookahead, t);
    }

    verbose("   token( %s )", t.d.string.str);
}

/* Break array abstraction to move data after cursor to the front, as we
 * consume tokens towards the end of list.
 */
static void rewind_lookahead_buffer(void)
{
    unsigned remaining;
    assert(array_len(&lookahead) >= cursor);

    remaining = array_len(&lookahead) - cursor;
    if (remaining) {
        memmove(
            lookahead.data,
            lookahead.data + cursor,
            remaining * sizeof(*lookahead.data));
    }

    array_len(&lookahead) = remaining;
    cursor = 0;
}

/* Consume at least one line, up until the final newline or end of file.
 * Fill up lookahead buffer and reset cursor. In case of end of input,
 * put END tokens in remaining lookahead slots.
 */
static void preprocess_line(void)
{
    struct token t;
    struct token *line, *expanded;

    ensure_initialized();
    rewind_lookahead_buffer();
    do {
        t = get_token();
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
                line = read_complete_line(t, 1);
                preprocess_directive(line);
            } else {
                line_buffer = NULL;
            }
        } else {
            assert(in_active_block());
            line = read_complete_line(t, 0);
            expanded = expand(line);
            line = expanded;
            while (line->token != END) {
                if (line->token != NEWLINE || preserve_whitespace) {
                    if (line->token != NEWLINE)
                        t = *line;
                    add_to_lookahead(*line);
                }
                line++;
            }
            free(expanded);
        }
    } while (
        (array_len(&lookahead) < K || t.token == STRING) && t.token != END);

    while (array_len(&lookahead) < K) {
        assert(t.token == END);
        add_to_lookahead(t);
    }
}

struct token next(void)
{
    if (cursor + K >= array_len(&lookahead)) {
        preprocess_line();
    }

    return array_get(&lookahead, cursor++);
}

struct token peek(void)
{
    return peekn(1);
}

struct token peekn(unsigned n)
{
    assert(n && n <= K);

    if (!array_len(&lookahead)) {
        /* If peek() is the first call made, make sure there is an
         * initial call to populate the lookahead buffer. */
        preprocess_line();
    }

    return array_get(&lookahead, cursor + n - 1);
}

struct token consume(enum token_type type)
{
    struct token t = next();

    if (t.token != type) {
        if (basic_token[type].d.string.str)
            error("Unexpected token '%s', expected '%s'.",
                t.d.string.str, basic_token[type].d.string.str);
        else
            error("Unexpected token '%s', expected %s.", t.d.string.str,
                (type == IDENTIFIER) ? "identifier" :
                (type == NUMBER) ? "number" : "string");
        exit(1);
    }

    return t;
}

void preprocess(FILE *output)
{
    struct token t;
    struct string s;

    preserve_whitespace = 1;
    while ((t = next()).token != END) {
        if (t.leading_whitespace) {
            fprintf(output, "%*s", t.leading_whitespace, " ");
        }
        if (t.token == STRING) {
            fprintstr(output, t.d.string); 
        } else {
            s = tokstr(t);
            fprintf(output, "%s", s.str);
        }
    }
}
