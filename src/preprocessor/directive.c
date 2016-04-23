#include "directive.h"
#include "input.h"
#include "macro.h"
#include "tokenize.h"
#include <lacc/array.h>
#include <lacc/cli.h>

#include <assert.h>

#define IDENT(s) {IDENTIFIER, 0, {{s, sizeof(s) - 1}}}

struct token
    ident__include = IDENT("include"),
    ident__defined = IDENT("defined"),
    ident__define = IDENT("define"),
    ident__ifndef = IDENT("ifndef"),
    ident__ifdef = IDENT("ifdef"),
    ident__undef = IDENT("undef"),
    ident__elif = IDENT("elif"),
    ident__endif = IDENT("endif"),
    ident__error = IDENT("error");

/* Keep stack of branch conditions for #if, #elif and #endif. Push and
 * pop results of evaluating expressions.
 */
static array_of(int) branch_stack;

static void push(int c)
{
    array_push_back(&branch_stack, c);
}

static int pop(void)
{
    int val;

    if (!array_len(&branch_stack)) {
        error("Unmatched #endif directive.");
        exit(1);
    }

    val = array_pop_back(&branch_stack);
    if (!array_len(&branch_stack)) {
        array_clear(&branch_stack);
    }

    return val;
}

int in_active_block(void)
{
    return array_len(&branch_stack) ?
        array_get(&branch_stack, array_len(&branch_stack) - 1) :
        1;
}

static void expect(const struct token *list, int token)
{
    if (list->token != token) {
        assert(basic_token[token].d.string.str);
        error("Expected '%s', but got '%s'.",
            basic_token[token].d.string.str, list->d.string.str);
        exit(1);
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
        a = a ? b : c;
    }

    *endptr = list;
    return a;
}

static void preprocess_include(const struct token line[])
{
    struct token t = {STRING, 0, {{""}}};

    if (line->token == STRING) {
        include_file(line->d.string.str);
    } else if (line->token == '<') {
        line++;
        while (line->token != NEWLINE) {
            if (line->token == '>') {
                break;
            }
            t = pastetok(t, *line++);
        }

        if (!t.d.string.len || line->token != '>') {
            error("Invalid include directive.");
            exit(1);
        }

        include_system_file(t.d.string.str);
    }
}

static struct macro preprocess_define(
    const struct token *line,
    const struct token **endptr)
{
    struct macro macro = {{0}};
    struct token param = {PARAM};
    TokenArray params = {0};
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
            array_push_back(&params, *line++);
            if (line->token != ',')
                break;
            line++;
        }
        expect(line, ')');
        line++;
    }

    macro.params = array_len(&params);

    while (line->token != NEWLINE) {
        assert(line->token != END);
        param.d.number.val.i = -1;
        if (line->token == IDENTIFIER && macro.type == FUNCTION_LIKE) {
            for (i = 0; i < macro.params; ++i) {
                if (!tok_cmp(*line, array_get(&params, i))) {
                    param.d.number.val.i = i;
                    break;
                }
            }
        }
        if (param.d.number.val.i != -1) {
            array_push_back(&macro.replacement, param);
        } else {
            array_push_back(&macro.replacement, *line);
        }
        line++;
    }

    *endptr = line;
    array_clear(&params);
    return macro;
}

void preprocess_directive(TokenArray *array)
{
    const struct token *line = array->data;

    if (line->token == IF || !tok_cmp(*line, ident__elif)) {
        /* Perform macro expansion only for if and elif directives,
         * before doing the expression parsing. */
        expand(array);
        line = array->data;
    }

    if (line->token == IF) {
        /* Expressions are not necessarily valid in dead blocks, for
         * example can function-like macros be undefined. */
        if (in_active_block()) {
            push(expression(line + 1, &line));
        } else {
            push(0);
        }
    } else if (line->token == ELSE) {
        push(!pop() && in_active_block());
    } else if (!tok_cmp(*line, ident__elif)) {
        if (!pop() && in_active_block()) {
            push(expression(line + 1, &line));
        } else {
            push(0);
        }
    } else if (!tok_cmp(*line, ident__endif)) {
        pop();
    } else if (!tok_cmp(*line, ident__ifndef)) {
        expect(++line, IDENTIFIER);
        push(!definition(*line) && in_active_block());
    } else if (!tok_cmp(*line, ident__ifdef)) {
        expect(++line, IDENTIFIER);
        push(definition(*line) && in_active_block());
    } else if (in_active_block()) {
        if (!tok_cmp(*line, ident__define)) {
            define(preprocess_define(line + 1, &line));
        } else if (!tok_cmp(*line, ident__undef)) {
            expect(++line, IDENTIFIER);
            undef(*line);
        } else if (!tok_cmp(*line, ident__include)) {
            preprocess_include(line + 1);
        } else if (!tok_cmp(*line, ident__error)) {
            array->data++;
            array->length--;
            error("%s", stringify(array).d.string.str);
            exit(1);
        }
    }
}
