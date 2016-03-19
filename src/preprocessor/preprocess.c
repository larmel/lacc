#include "input.h"
#include "macro.h"
#include "preprocess.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/cli.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Helper structure and functions for aggregating tokens into a line
 * before preprocessing.
 */
struct builder {
    struct token *elem;
    size_t length;
};

/* Buffer of preprocessed tokens, ready to be consumed by the parser.
 * Configured to hold at least K tokens, enabling LL(K) parsing.
 *
 * For the K&R grammar, it is sufficient to have K = 2.
 *
 * Cursor points to current position in lookahead buffer, the token to
 * be returned by next().
 */
static struct token *lookahead;
static size_t length;
static size_t cursor;

static const int K = 2;

/* Toggle for producing preprocessed output (-E).
 */
static int preserve_whitespace;

/* Push and pop branch conditions for #if, #elif and #endif.
 */
static struct {
    int *condition;
    size_t length;
    size_t cap;
} branch_stack;

static int expression(const struct token *list, const struct token **endptr);

static void cnd_push(int c) {
    if (branch_stack.length == branch_stack.cap) {
        branch_stack.cap += 16;
        branch_stack.condition = 
            realloc(branch_stack.condition, branch_stack.cap * sizeof(int));
    }

    branch_stack.condition[branch_stack.length++] = c;
}

static int cnd_peek(void) {
    return branch_stack.length ? 
        branch_stack.condition[branch_stack.length - 1] : 1;
}

static int cnd_pop(void) {
    if (!branch_stack.length)
        error("Unmatched #endif directive.");

    return branch_stack.condition[--branch_stack.length];
}

static void cleanup(void)
{
    if (lookahead) {
        free(lookahead);
        lookahead = NULL;
        length = 0;
        cursor = 0;
    }

    if (branch_stack.condition) {
        free(branch_stack.condition);
        branch_stack.condition = NULL;
        branch_stack.length = 0;
        branch_stack.cap = 0;
    }
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

static void list_append(struct builder *list, struct token t)
{
    list->length++;
    list->elem = realloc(list->elem, sizeof(*list->elem) * list->length);
    list->elem[list->length - 1] = t;
}

/* Keep track of the nesting depth of macro arguments. For example;
 * MAX( MAX(10, 12), 20 ) should complete on the last parenthesis, which
 * makes the expression balanced. Read lines until full macro invocation
 * is included.
 */
static void read_macro_invocation(
    struct builder *list,
    const struct macro *macro)
{
    int nesting;
    struct token t;
    assert(macro->type == FUNCTION_LIKE);

    t = get_token();
    list_append(list, t);
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
        list_append(list, t);
    }
    if (nesting) {
        error("Unbalanced invocation of macro '%s'.", macro->name.strval.str);
        exit(1);
    }
}

/* Replace 'defined name' and 'defined (name)' with 0 or 1 constants.
 */
static void read_defined_operator(struct builder *list)
{
    int is_parens = 0;
    struct token t = get_token();

    if (t.token == '(') {
        t = get_token();
        is_parens = 1;
    }
    if (t.token != IDENTIFIER) {
        error("Expected identifier in 'defined' clause, but got '%s'",
            t.strval.str);
        exit(1);
    }
    if (definition(t)) {
        t.intval = 1;
        t.strval = str_init("1");
    } else {
        t.intval = 0;
        t.strval = str_init("0");
    }
    t.token = INTEGER_CONSTANT;
    list_append(list, t);
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
    ident.strval = str_init(name);
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
    const struct macro *def;
    struct builder line = {0};
    int is_expandable = 1,
        is_directive = (t.token == '#');

    if (is_directive) {
        list_append(&line, t);
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
                list_append(&line, t);
                read_macro_invocation(&line, def);
            } else {
                list_append(&line, t);
            }
        } else {
            list_append(&line, t);
        }
        t = get_token();
    }

    if (preserve_whitespace && t.token == NEWLINE)
        list_append(&line, t);

    list_append(&line, basic_token[END]);
    return line.elem;
}

static void expect(const struct token *list, int token)
{
    if (list->token != token) {
        assert(basic_token[token].strval.str);
        error("Expected '%s', but got '%s'.",
            basic_token[token].strval.str, list->strval.str);
    }
}

static int eval_primary(
    const struct token *list,
    const struct token **endptr)
{
    int value = 0;

    switch (list->token) {
    case INTEGER_CONSTANT:
        value = list->intval;
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
        error("Invalid primary expression '%s'.", list->strval.str);
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

    assert(a.strval.str);
    assert(b.strval.str);

    t.strval.len = a.strval.len + b.strval.len;
    str = calloc(t.strval.len + 1, sizeof(*str));
    memcpy(str, a.strval.str, a.strval.len);
    memcpy(str + a.strval.len, b.strval.str, b.strval.len);

    t.strval = str_register(str, t.strval.len);
    free(str);
    return t;
}

static void preprocess_include(const struct token line[])
{
    struct token t = {STRING, 0, {"", 0}};

    if (line->token == STRING) {
        include_file(line->strval.str);
    } else if (line->token == '<') {
        line++;
        while (line->token != END) {
            if (line->token == '>') {
                break;
            }
            t = pastetok(t, *line++);
        }

        if (!t.strval.len) {
            error("Invalid include directive.");
            exit(1);
        }

        assert(line->token == '>');
        include_system_file(t.strval.str);
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
            error("%s", stringify(line).strval.str);
            exit(1);
        }
    }

    free(original);
}

static void add_to_lookahead(struct token t)
{
    size_t i = length;
    int added = 0;

    /* Combine adjacent string literals. This step is done after
     * preprocessing and macro expansion; logic in preprocess_line will
     * guarantee that we keep preprocessing lines and filling up the
     * lookahead buffer for as long as there can be continuations. */
    if (t.token == STRING) {
        if (i-- && lookahead[i].token == STRING) {
            lookahead[i] = pastetok(lookahead[i], t);
            added = 1;
        }
    }

    if (!added) {
        length++;
        lookahead = realloc(lookahead, length * sizeof(*lookahead));
        lookahead[length - 1] = t;
    }

    verbose("   token( %s )", t.strval.str);
}

static void rewind_lookahead_buffer(void)
{
    size_t remaining;
    assert(length >= cursor);

    remaining = length - cursor;
    if (remaining) {
        lookahead =
            memmove(lookahead,
                lookahead + cursor, remaining * sizeof(*lookahead));
    }
    length = remaining;
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
    } while ((length < K || t.token == STRING) && t.token != END);

    /* Fill remainder of lookahead buffer. */
    while (length < K) {
        assert(t.token == END);
        add_to_lookahead(t);
    }
}

struct token next(void)
{
    if (cursor + K >= length) {
        preprocess_line();
    }

    return lookahead[cursor++];
}

struct token peek(void)
{
    return peekn(1);
}

struct token peekn(unsigned n)
{
    assert(n && n <= K);

    if (!length) {
        /* If peek() is the first call made, make sure there is an
         * initial call to populate the lookahead buffer. */
        preprocess_line();
    }

    return lookahead[cursor + n - 1];
}

struct token consume(enum token_type type)
{
    struct token t = next();

    if (t.token != type) {
        if (basic_token[type].strval.str)
            error("Unexpected token '%s', expected '%s'.",
                t.strval.str, basic_token[type].strval.str);
        else
            error("Unexpected token '%s', expected %s.", t.strval.str,
                (type == IDENTIFIER) ? "identifier" :
                (type == INTEGER_CONSTANT) ? "number" : "string");
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
            fprintstr(output, t.strval);
            break;
        case INTEGER_CONSTANT:
            fprintf(output, "%ld", t.intval);
            break;
        default:
            fprintf(output, "%s", t.strval.str);
            break;
        }
        t = next();
    }
}
