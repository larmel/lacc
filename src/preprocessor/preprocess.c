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

/* Helper structure and functions for aggregating tokens into a line
 * before preprocessing.
 */
typedef array_of(struct token) TokenArray;

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

/* Push and pop branch conditions for #if, #elif and #endif.
 */
static array_of(int) branch_stack;

static void cnd_push(int c)
{
    array_push_back(&branch_stack, c);
}

static int cnd_peek(void)
{
    return array_len(&branch_stack) ?
        array_get(&branch_stack, array_len(&branch_stack) - 1) :
        1;
}

static int cnd_pop(void)
{
    if (!array_len(&branch_stack)) {
        error("Unmatched #endif directive.");
        exit(1);
    }
    return array_pop_back(&branch_stack);
}

static void cleanup(void)
{
    array_clear(&branch_stack);
    array_clear(&lookahead);
}

static struct token get_token(void)
{
    static char *line;

    struct token r;
    char *endptr;

    if (!line && getprepline(&line) == -1) {
        r = basic_token[END];
    } else {
        r = tokenize(line, &endptr);
        line = endptr;
        if (r.token == END) {
            /* Newlines are removed by getprepline, and never present in
             * the input data. Instead intercept end of string, which
             * represents end of line. */
            line = NULL;
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

/* Determine if token is an identifier with the given name, returning 0
 * otherwise.
 */
static int is_ident(struct token tok, const char *name)
{
    struct token ident = {IDENTIFIER};
    ident.d.string = str_init(name);
    return !tok_cmp(tok, ident);
}

/* Read tokens until reaching newline or eof. If initial token is '#',
 * stop on newline. Otherwise make sure macro invocations spanning
 * multiple lines are joined, and replace 'defined' directives with
 * constants.
 *
 * Returns a buffer containing all necessary tokens to preprocess a
 * line.
 */
static struct token *read_complete_line(struct token t)
{
    TokenArray line = {0};
    const struct macro *def;
    int is_expandable = 1,
        is_directive = (t.token == '#');

    if (is_directive) {
        array_push_back(&line, t);
        t = get_token();
        is_expandable = (t.token == IF) || is_ident(t, "elif");
    }

    while (t.token != NEWLINE && t.token != END) {
        if (t.token == IDENTIFIER) {
            if (is_ident(t, "defined") && is_directive && is_expandable) {
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

static void expect(const struct token *list, int token)
{
    if (list->token != token) {
        assert(basic_token[token].d.string.str);
        error("Expected '%s', but got '%s'.",
            basic_token[token].d.string.str, list->d.string.str);
    }
}

static int expression(const struct token *list, const struct token **endptr);

static int eval_primary(
    const struct token *list,
    const struct token **endptr)
{
    int value = 0;

    switch (list->token) {
    case NUMBER:
        value = list->d.number.val.i;
        break;
    case IDENTIFIER:
        /* Macro expansions should already have been done. Stray
         * identifiers are interpreted as zero constants. */
        assert(!definition(*list));
        break;
    case '(':
        value = expression(list + 1, &list);
        expect(list, ')');
        break;
    default:
        error("Invalid primary expression '%s'.", list->d.string.str);
        break;
    }
    *endptr = list + 1;
    return value;
}

static int eval_unary(const struct token *list, const struct token **endptr)
{
    switch (list->token) {
    case '+':
        return + eval_unary(list + 1, endptr);
    case '-':
        return - eval_unary(list + 1, endptr);
    case '~':
        return ~ eval_unary(list + 1, endptr);
    case '!':
        return ! eval_unary(list + 1, endptr);
    default:
        return eval_primary(list, endptr);
    }
}

static int eval_multiplicative(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_unary(list, &list),
        done = 0;

    do {
        switch (list->token) {
        case '*':
            val = val * eval_unary(list + 1, &list);
            break;
        case '/':
            val = val / eval_unary(list + 1, &list);
            break;
        case '%':
            val = val % eval_unary(list + 1, &list);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    *endptr = list;
    return val;
}

static int eval_additive(const struct token *list, const struct token **endptr)
{
    int val = eval_multiplicative(list, &list),
        done = 0;

    do {
        switch (list->token) {
        case '+':
            val = val + eval_multiplicative(list + 1, &list);
            break;
        case '-':
            val = val - eval_multiplicative(list + 1, &list);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    *endptr = list;
    return val;
}

static int eval_shift(const struct token *list, const struct token **endptr)
{
    int val = eval_additive(list, &list),
        done = 0;

    do {
        switch (list->token) {
        case LSHIFT:
            val = val << eval_additive(list + 1, &list);
            break;
        case RSHIFT:
            val = val >> eval_additive(list + 1, &list);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);
    *endptr = list;
    return val;
}

static int eval_relational(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_shift(list, &list),
        done = 0;

    do {
        switch (list->token) {
        case '<':
            val = val < eval_shift(list + 1, &list);
            break;
        case '>':
            val = val > eval_shift(list + 1, &list);
            break;
        case LEQ:
            val = val <= eval_shift(list + 1, &list);
            break;
        case GEQ:
            val = val >= eval_shift(list + 1, &list);
            break;
        default:
            done = 1;
            break;
        }
    } while (!done);

    *endptr = list;
    return val;
}

static int eval_equality(const struct token *list, const struct token **endptr)
{
    int val = eval_relational(list, &list);

    if (list->token == EQ) {
        val = (val == eval_equality(list + 1, &list));
    } else if (list->token == NEQ) {
        val = (val != eval_equality(list + 1, &list));
    }

    *endptr = list;
    return val;
}

static int eval_and(const struct token *list, const struct token **endptr)
{
    int val = eval_equality(list, &list);

    if (list->token == '&')
        val = eval_and(list + 1, &list) & val;

    *endptr = list;
    return val;
}

static int eval_exclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_and(list, &list);

    if (list->token == '^')
        val = eval_exclusive_or(list + 1, &list) ^ val;

    *endptr = list;
    return val;
}

static int eval_inclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_exclusive_or(list, &list);

    if (list->token == '|')
        val = eval_inclusive_or(list + 1, &list) | val;

    *endptr = list;
    return val;
}

static int eval_logical_and(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_inclusive_or(list, &list);

    if (list->token == LOGICAL_AND)
        val = eval_logical_and(list + 1, &list) && val;

    *endptr = list;
    return val;
}

static int eval_logical_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_logical_and(list, &list);

    if (list->token == LOGICAL_OR)
        val = eval_logical_or(list + 1, &list) || val;

    *endptr = list;
    return val;
}

static int expression(const struct token *list, const struct token **endptr)
{
    int a = eval_logical_or(list, &list), b, c;

    if (list->token == '?') {
        b = expression(list + 1, &list);
        expect(list, ':');
        c = expression(list + 1, &list);
        return a ? b : c;
    }

    *endptr = list;
    return a;
}

static struct macro preprocess_define(
    const struct token *line,
    const struct token **endptr)
{
    struct macro macro = {{0}};
    struct token *params = NULL;
    int i;

    expect(line, IDENTIFIER);
    macro.name = *line++;
    macro.type = OBJECT_LIKE;

    /* Function-like macro iff parenthesis immediately after
     * identifier. */
    if (line->token == '(' && !line->leading_whitespace) {
        macro.type = FUNCTION_LIKE;
        line++;
        while (line->token != ')') {
            if (line->token != IDENTIFIER) {
                error("Invalid macro parameter, expected identifer.");
                exit(1);
            }
            macro.params++;
            params = realloc(params, macro.params * sizeof(*params));
            params[macro.params - 1] = *line++;
            if (line->token != ',')
                break;
            line++;
        }
        expect(line, ')');
        line++;
    }

    while (line->token != END && line->token != NEWLINE) {
        macro.size++;
        macro.replacement = 
            realloc(macro.replacement, macro.size * sizeof(*macro.replacement));
        macro.replacement[macro.size - 1].token = *line;
        macro.replacement[macro.size - 1].param = 0;
        if (line->token == IDENTIFIER && macro.type == FUNCTION_LIKE) {
            for (i = 0; i < macro.params; ++i) {
                if (!tok_cmp(*line, params[i])) {
                    macro.replacement[macro.size - 1].param = i + 1;
                    break;
                }
            }
        }
        line++;
    }

    *endptr = line;
    free(params);
    return macro;
}

/* Concatenate tokens to produce a new string token.
 */
static struct token pastetok(struct token a, struct token b)
{
    char *str;
    struct token t = {STRING};

    assert(a.d.string.str);
    assert(b.d.string.str);

    t.d.string.len = a.d.string.len + b.d.string.len;
    str = calloc(t.d.string.len + 1, sizeof(*str));
    memcpy(str, a.d.string.str, a.d.string.len);
    memcpy(str + a.d.string.len, b.d.string.str, b.d.string.len);

    t.d.string = str_register(str, t.d.string.len);
    free(str);
    return t;
}

static void preprocess_include(const struct token line[])
{
    struct token t = {STRING, 0, {{""}}};

    if (line->token == STRING) {
        include_file(line->d.string.str);
    } else if (line->token == '<') {
        line++;
        while (line->token != END) {
            if (line->token == '>') {
                break;
            }
            t = pastetok(t, *line++);
        }

        if (!t.d.string.len) {
            error("Invalid include directive.");
            exit(1);
        }

        assert(line->token == '>');
        include_system_file(t.d.string.str);
    }
}

/* Preprocess a line starting with a '#' directive. Takes ownership of
 * input. Assume input is END terminated, and not containing newline.
 */
static void preprocess_directive(struct token *original)
{
    const struct token *line = original;

    expect(line, '#');
    line++;
    if (line->token == IF || is_ident(*line, "elif")) {
        /* Perform macro expansion only for if and elif directives,
         * before doing the expression parsing. */
        original = expand(original);
        line = original;
        expect(line, '#');
        line++;
    }

    if (line->token == IF) {
        /* Expressions are not necessarily valid in dead blocks, for
         * example can function-like macros be undefined. */
        if (cnd_peek()) {
            cnd_push(expression(line + 1, &line));
        } else {
            cnd_push(0);
        }
    } else if (line->token == ELSE) {
        cnd_push(!cnd_pop() && cnd_peek());
    } else if (is_ident(*line, "elif")) {
        if (!cnd_pop() && cnd_peek()) {
            cnd_push(expression(line + 1, &line));
        } else {
            cnd_push(0);
        }
    } else if (is_ident(*line, "endif")) {
        cnd_pop();
    } else if (is_ident(*line, "ifndef")) {
        expect(++line, IDENTIFIER);
        cnd_push(!definition(*line) && cnd_peek());
    } else if (is_ident(*line, "ifdef")) {
        expect(++line, IDENTIFIER);
        cnd_push(definition(*line++) && cnd_peek());
    } else if (cnd_peek()) {
        if (is_ident(*line, "define")) {
            define(preprocess_define(line + 1, &line));
        } else if (is_ident(*line, "undef")) {
            expect(++line, IDENTIFIER);
            undef(*line++);
        } else if (is_ident(*line, "include")) {
            preprocess_include(line + 1);
        } else if (is_ident(*line, "error")) {
            line++;
            error("%s", stringify(line).d.string.str);
            exit(1);
        }
    }

    free(original);
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
 * Fill up lookahead buffer and reset cursor.
 */
static void preprocess_line(void)
{
    static int call_cleanup;
    struct token t = {0};

    if (!call_cleanup) {
        call_cleanup = 1;
        atexit(cleanup);
    }

    rewind_lookahead_buffer();

    do {
        struct token
            *line, *expanded;

        t = get_token();
        if (t.token == '#') {
            line = read_complete_line(t);
            preprocess_directive(line);
        } else if (cnd_peek()) {
            line = read_complete_line(t);
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
        } else {
            while (t.token != NEWLINE && t.token != END) {
                t = get_token();
            }
        }
    } while (
        (array_len(&lookahead) < K || t.token == STRING) && t.token != END);

    /* Fill remainder of lookahead buffer. */
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

    preserve_whitespace = 1;
    t = next();
    while (t.token != END) {
        if (t.leading_whitespace > 0) {
            fprintf(output, "%*s", t.leading_whitespace, " ");
        }
        switch (t.token) {
        case STRING:
            fprintstr(output, t.d.string);
            break;
        case NUMBER:
            if (t.d.number.type->type == T_UNSIGNED)
                fprintf(output, "%luu", t.d.number.val.u);
            else
                fprintf(output, "%ld", t.d.number.val.i);
            if (t.d.number.type->size == 8)
                fputc('l', output);
            else
                assert(t.d.number.type->size == 4);
            break;
        default:
            fprintf(output, "%s", t.d.string.str);
            break;
        }
        t = next();
    }
}
