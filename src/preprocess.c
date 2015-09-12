#include "error.h"
#include "input.h"
#include "macro.h"
#include "string.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Helper structure and functions for aggregating tokens into a line before
 * preprocessing.
 */
struct builder {
    struct token *elem;
    size_t length;
};

static void list_append(struct builder *list, struct token t)
{
    list->length++;
    list->elem = realloc(list->elem, sizeof(*list->elem) * list->length);
    list->elem[list->length - 1] = t;
}

static struct token get_next(struct builder *list)
{
    struct token t = get_preprocessing_token();
    while (t.token == SPACE) {
        list_append(list, t);
        t = get_preprocessing_token();
    }
    return t;
}

/* Skip through whitespace and add token of expected type. Whitespace is also
 * added.
 */
static struct token expect_next(struct builder *list, int type)
{
    struct token t = get_next(list);
    if (t.token != type) {
        error("Expected token '%c', but got '%s'.", (char) type, t.strval);
        exit(1);
    }
    return t;
}

/* Keep track of the nesting depth of macro arguments. For example MAX( MAX(10,
 * 12), 20 ) should complete on the last parenthesis, which makes the expression
 * balanced. Read lines until full macro invocation is included.
 */
static void read_macro_invocation(
    struct builder *list,
    const struct macro *macro)
{
    int nesting = 1;
    struct token t = expect_next(list, '(');
    assert(macro->type == FUNCTION_LIKE);

    list_append(list, t);
    while (nesting) {
        t = get_next(list);
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
        list_append(list, t);
    }
    if (nesting) {
        error("Unbalanced invocation of macro '%s'.", macro->name.strval);
        exit(1);
    }
}

/* Replace 'defined name' and 'defined (name)' with 0 or 1 constants.
 */
static void read_defined_operator(struct builder *list)
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
    list_append(list, t);
    if (is_parens) {
        expect_next(list, ')');
    }
}

static struct token end_token = {END, "$"};

/* Read tokens until reaching newline or eof. If initial token is '#', stop on
 * newline. Otherwise make sure macro invocations spanning multiple lines are
 * joined, and replace 'defined' directives with constants.
 *
 * Returns a buffer containing all necessary tokens to preprocess a line.
 */
static struct token *read_complete_line(struct token t)
{
    struct builder line = {0};
    int is_expandable = 1,
        is_directive = (t.token == '#');
    const struct macro *def;

    if (is_directive) {
        list_append(&line, t);
        t = get_next(&line);
        is_expandable =
            (t.token == IF ||
                (t.token == IDENTIFIER && !strcmp("elif", t.strval)));
    }

    while (t.token != NEWLINE && t.token != END) {
        if (t.token == IDENTIFIER) {
            if (!strcmp("defined", t.strval) && is_directive && is_expandable) {
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
        t = get_preprocessing_token();
    }

    list_append(&line, end_token);
    return line.elem;
}

static const struct token *skip_ws(const struct token *list)
{
    while (list->token == SPACE) list++;
    return list;
}

static const struct token *skip_to(const struct token *list, int token)
{
    while (list->token == SPACE) list++;
    if (list->token != token) {
        error("Unexpected '%c', expected '%c'.", list->strval);
    }
    return list;
}

static int expression(const struct token *list, const struct token **endptr);

static int eval_primary(
    const struct token *list,
    const struct token **endptr)
{
    int value = 0;

    list = skip_ws(list);
    switch (list->token) {
    case INTEGER_CONSTANT:
        value = list->intval;
        break;
    case IDENTIFIER:
        /* Macro expansions should already have been done. Stray identifiers are
         * interpreted as zero constants. */
        assert(!definition(*list));
        break;
    case '(':
        value = expression(list + 1, &list);
        list = skip_to(list, ')');
        break;
    default:
        error("Invalid primary expression '%s'.", list->strval);
        break;
    }
    *endptr = list + 1;
    return value;
}

static int eval_unary(const struct token *list, const struct token **endptr)
{
    list = skip_ws(list);
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
        list = skip_ws(list);
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
        list = skip_ws(list);
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
        list = skip_ws(list);
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
        list = skip_ws(list);
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

    list = skip_ws(list);
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

    list = skip_ws(list);
    if (list->token == '&') {
        val = eval_and(list + 1, &list) & val;
    }
    *endptr = list;
    return val;
}

static int eval_exclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_and(list, &list);

    list = skip_ws(list);
    if (list->token == '^') {
        val = eval_exclusive_or(list + 1, &list) ^ val;
    }
    *endptr = list;
    return val;
}

static int eval_inclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_exclusive_or(list, &list);

    list = skip_ws(list);
    if (list->token == '|') {
        val = eval_inclusive_or(list + 1, &list) | val;
    }
    *endptr = list;
    return val;
}

static int eval_logical_and(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_inclusive_or(list, &list);

    list = skip_ws(list);
    if (list->token == LOGICAL_AND) {
        val = eval_logical_and(list + 1, &list) && val;
    }
    *endptr = list;
    return val;
}

static int eval_logical_or(
    const struct token *list,
    const struct token **endptr)
{
    int val = eval_logical_and(list, &list);

    list = skip_ws(list);
    if (list->token == LOGICAL_OR) {
        val = eval_logical_or(list + 1, &list) || val;
    }
    *endptr = list;
    return val;
}

static int expression(const struct token *list, const struct token **endptr)
{
    int a = eval_logical_or(list, &list), b, c;

    list = skip_ws(list);
    if (list->token == '?') {
        b = expression(list + 1, &list);
        list = skip_to(list, ':');
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

    line = skip_to(line, IDENTIFIER);
    macro.name = *line++;
    macro.type = OBJECT_LIKE;

    /* Function-like macro iff parenthesis immediately after identifier. */
    if (line->token == '(') {
        macro.type = FUNCTION_LIKE;
        line++;
        while ((line = skip_ws(line))->token != ')') {
            if (line->token != IDENTIFIER) {
                error("Invalid macro parameter, expected identifer.");
                exit(1);
            }
            macro.params++;
            params = realloc(params, macro.params * sizeof(*params));
            params[macro.params - 1] = *line++;
            if ((line = skip_ws(line))->token != ',') {
                break;
            }
            line++;
        }
        line = skip_to(line, ')') + 1;
    }

    /* First whitespace not part of replacement list. */
    line = skip_ws(line);

    /* Everything else is... */
    while (line->token != END) {
        macro.size++;
        macro.replacement = 
            realloc(macro.replacement, macro.size * sizeof(*macro.replacement));
        macro.replacement[macro.size - 1].token = *line;
        macro.replacement[macro.size - 1].param = 0;
        if (line->token == IDENTIFIER) {
            int i;
            for (i = 0; i < macro.params; ++i) {
                if (!strcmp(line->strval, params[i].strval)) {
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

static char *pastetok(char *buf, struct token t)
{
    size_t len;

    if (!buf) {
        buf = calloc(16, sizeof(*buf));
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

static void preprocess_include(const struct token line[])
{
    line = skip_ws(line);
    if (line->token == STRING) {
        include_file(line->strval);
    } else if (line->token == '<') {
        char *path = NULL;

        line = skip_ws(line + 1);
        while (line->token != END) {
            if (line->token == '>') {
                break;
            }
            line = skip_ws(line);
            path = pastetok(path, *line++);
        }

        if (!path) {
            error("Invalid include directive.");
            exit(1);
        }
        assert(line->token == '>');
        include_system_file(path);
    }
}

/* Push and pop branch conditions for #if, #elif and #endif.
 */
static struct {
    int *condition;
    size_t length;
    size_t cap;
} branch_stack;

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
    if (!branch_stack.length) {
        error("Unmatched #endif directive.");
    }
    return branch_stack.condition[--branch_stack.length];
}

/* Preprocess a line starting with a '#' directive. Takes ownership of input.
 *
 * Assumes input is END terminated, and not containing newline.
 */
static void preprocess_directive(const struct token *line)
{
    struct token *expanded = NULL;

    line = skip_to(line, '#');
    line = skip_ws(line + 1);
    if (line->token == IF ||
        (line->token == IDENTIFIER && !strcmp("elif", line->strval)))
    {
        /* Perform macro expansion only for if and elif directives, before doing
         * the expression parsing. */
        expanded = expand(line);
        line = expanded;
    }

    if (line->token == IF) {
        /* Expressions are not necessarily valid in dead blocks, for example
         * can function-like macros be undefined. */
        if (cnd_peek()) {
            cnd_push(expression(line + 1, &line));
        } else {
            cnd_push(0);
        }
    } else if (line->token == ELSE) {
        cnd_push(!cnd_pop() && cnd_peek());
    } else if (line->token == IDENTIFIER && !strcmp("elif", line->strval)) {
        if (!cnd_pop() && cnd_peek()) {
            cnd_push(expression(line + 1, &line));
        } else {
            cnd_push(0);
        }
    } else if (line->token == IDENTIFIER && !strcmp("endif", line->strval)) {
        cnd_pop();
    } else if (line->token == IDENTIFIER && !strcmp("ifndef", line->strval)) {
        line = skip_to(line + 1, IDENTIFIER);
        cnd_push(!definition(*line) && cnd_peek());
    } else if (line->token == IDENTIFIER && !strcmp("ifdef", line->strval)) {
        line = skip_to(line + 1, IDENTIFIER);
        cnd_push(definition(*line++) && cnd_peek());
    } else if (cnd_peek() && line->token == IDENTIFIER) {
        if (!strcmp("define", line->strval)) {
            define(preprocess_define(line + 1, &line));
        } else if (!strcmp("undef", line->strval)) {
            line = skip_to(line + 1, IDENTIFIER);
            undef(*line++);
        } else if (!strcmp("include", line->strval)) {
            preprocess_include(line + 1);
        } else if (!strcmp("error", line->strval)) {
            line = skip_ws(line + 1);
            error("%s", stringify(line).strval);
            exit(1);
        }
    }

    free(expanded);
}

/* Buffer of preprocessed tokens, ready to be consumed by the parser. Configured
 * to hold at least K tokens, enabling LL(K) parsing.
 *
 * For the K&R grammar, it is sufficient to have K = 2.
 *
 * Cursor points to current position in lookahead buffer, the token to be
 * returned by next().
 */
static struct token *lookahead;
static size_t length;
static size_t cursor;

static const int K = 2;

/* Toggle for producing preprocessed output (-E).
 */
static int preserve_whitespace;

/* Add preprocessed token to lookahead buffer.
 */
static void add(struct token t)
{
    extern int VERBOSE;
    size_t i = length;
    char *str = NULL;

    /* Combine adjacent string literals. This step is done after preprocessing
     * and macro expansion; logic in preprocess_line will guarantee that we keep
     * preprocessing lines and filling up the lookahead buffer for as long as
     * there can be string continuations. */
    if (t.token == STRING) {
        while (i && lookahead[--i].token == SPACE)
            ;
        if (lookahead[i].token == STRING) {
            str = pastetok(str, lookahead[i]);
            str = pastetok(str, t);
            t.strval = str_register(str);
            lookahead[i] = t;
            free(str);
        }
    }

    if (!str) {
        length++;
        lookahead = realloc(lookahead, length * sizeof(*lookahead));
        lookahead[length - 1] = t;
    }

    if (VERBOSE) {
        printf("   token( %s )\n", t.strval);
    }
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

/* Consume at least one line, up until the final newline or end of file. Fill up
 * lookahead buffer and reset cursor.
 */
static void preprocess_line(void)
{
    struct token t = {0};

    rewind_lookahead_buffer();

    do {
        struct token
            *line = NULL,
            *expanded = NULL;

        do {
            t = get_preprocessing_token();
        } while (t.token == SPACE);

        if (t.token == '#') {
            line = read_complete_line(t);
            preprocess_directive(line);
            free(line);
        } else if (cnd_peek()) {
            line = read_complete_line(t);
            expanded = expand(line);
            free(line);
            line = expanded;
            while (line->token != END) {
                if (line->token != SPACE || preserve_whitespace) {
                    if (line->token != SPACE)
                        t = *line;
                    add(*line);
                }
                line++;
            }
            free(expanded);
        } else {
            while (t.token != NEWLINE && t.token != END) {
                t = get_preprocessing_token();
            }
        }
    } while ((length < K || t.token == STRING) && t.token != END);

    /* Fill remainder of lookahead buffer. */
    while (length < K) {
        assert(t.token == END);
        add(t);
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
    if (!length) {
        /* If peek() is the first call made, make sure there is an initial call
         * to populate the lookahead buffer. */
        preprocess_line();
    }
    return lookahead[cursor];
}

struct token peekn(unsigned n)
{
    assert(n && n <= K);

    if (!length) {
        preprocess_line();
    }
    return lookahead[cursor + n - 1];
}

struct token consume(int expected)
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
