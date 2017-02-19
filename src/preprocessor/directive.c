#include "directive.h"
#include "input.h"
#include "macro.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>

#define IDENT(s) {IDENTIFIER, 0, 1, 0, 0, {0}, {SHORT_STRING_INIT(s)}}

struct token
    ident__include = IDENT("include"),
    ident__defined = IDENT("defined"),
    ident__define = IDENT("define"),
    ident__ifndef = IDENT("ifndef"),
    ident__ifdef = IDENT("ifdef"),
    ident__undef = IDENT("undef"),
    ident__elif = IDENT("elif"),
    ident__endif = IDENT("endif"),
    ident__error = IDENT("error"),
    ident__VA_ARGS__ = IDENT("__VA_ARGS__");

enum state {
    /*
     * Default state, inside an active #if, #elif, #else, #ifdef, or
     * #ifndef directive.
     */
    BRANCH_LIVE,
    /*
     * A Previous branch in #if, #elif chain was taken. Everything
     * up until #endif is dead code, and new #elif directives will not
     * be computed.
     */
    BRANCH_DISABLED,
    /*
     * Dead code. New #else or #elif directives become live if evaluated
     * to true.
     */
    BRANCH_DEAD
};

struct number {
    Type type;
    union value val;
};

/*
 * Keep stack of branch conditions for #if, #elif and #endif. Push and
 * pop according to current and parent state, and result of evaluating
 * expressions.
 */
static array_of(enum state) branch_stack;

static void push(enum state c)
{
    array_push_back(&branch_stack, c);
}

static enum state pop(void)
{
    enum state val;

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
    enum state s = BRANCH_LIVE;
    if (array_len(&branch_stack)) {
        s = array_get(&branch_stack, array_len(&branch_stack) - 1);
    }

    return s == BRANCH_LIVE;
}

static void expect(const struct token *list, int token)
{
    String a, b;
    if (list->token != token) {
        a = tokstr(basic_token[token]);
        b = tokstr(*list);
        error("Expected '%s', but got '%s'.", str_raw(a), str_raw(b));
        exit(1);
    }
}

static struct number expression(
    const struct token *list,
    const struct token **endptr);

/*
 * Macro expansions should already have been done. Stray identifiers are
 * interpreted as zero constants.
 *
 * Normalize all values to be of type long or unsigned long. Floating
 * point numbers are not permitted by the standard.
 */
static struct number eval_primary(
    const struct token *list,
    const struct token **endptr)
{
    String s;
    struct token n;
    struct number num;

    switch (list->token) {
    case PREP_NUMBER:
        n = convert_preprocessing_number(*list);
        assert(n.token == NUMBER);
        num.type = n.type;
        num.val = n.d.val;
        break;
    case NUMBER:
        num.type = list->type;
        num.val = list->d.val;
        break;
    case '(':
        num = expression(list + 1, &list);
        expect(list, ')');
        break;
    default:
        if (!list->is_expandable) {
            s = tokstr(*list);
            error("Invalid primary expression '%s' in directive.", str_raw(s));
            exit(1);
        }
    case IDENTIFIER:
        assert(!definition(list->d.string));
        num.type = basic_type__long;
        num.val.i = 0;
        break;
    }

    *endptr = list + 1;
    if (!is_integer(num.type)) {
        error("Preprocessing number must be integer.");
        exit(1);
    } else if (size_of(num.type) != 8) {
        if (is_signed(num.type)) {
            num.val = convert(num.val, num.type, basic_type__long);
            num.type = basic_type__long;
        } else {
            num.val = convert(num.val, num.type, basic_type__unsigned_long);
            num.type = basic_type__unsigned_long;
        }
    }

    return num;
}

static struct number eval_unary(
    const struct token *list,
    const struct token **endptr)
{
    struct number r;

    switch (list->token) {
    case '+':
        r = eval_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = + r.val.i;
        }
        return r;
    case '-':
        r = eval_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = - r.val.i;
        } else {
            r.val.u = - r.val.u;
        }
        return r;
    case '~':
        r = eval_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ~ r.val.i;
        } else {
            r.val.u = ~ r.val.u;
        }
        return r;
    case '!':
        r = eval_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ! r.val.i;
        } else {
            r.val.u = ! r.val.u;
        }
        return r;
    default:
        return eval_primary(list, endptr);
    }
}

static int both_signed(struct number *l, struct number *r)
{
    Type type;

    if (!type_equal(l->type, r->type)) {
        type = usual_arithmetic_conversion(l->type, r->type);
        l->val = convert(l->val, l->type, type);
        l->type = type;
        r->val = convert(r->val, r->type, type);
        r->type = type;
    } else {
        type = l->type;
    }

    assert(is_integer(type));
    return is_signed(type);
}

static struct number eval_multiplicative(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_unary(list, &list);
    while (1) {
        switch (list->token) {
        case '*':
            r = eval_unary(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i *= r.val.i;
            } else {
                l.val.u *= r.val.u;
            }
            break;
        case '/':
            r = eval_unary(list + 1, &list);
            if (r.val.i == 0) {
                error("Division by zero.");
                exit(1);
            } else if (both_signed(&l, &r)) {
                l.val.i /= r.val.i;
            } else {
                l.val.u /= r.val.u;
            }
            break;
        case '%':
            r = eval_unary(list + 1, &list);
            if (r.val.i == 0) {
                error("Modulo by zero.");
                exit(1);
            } else if (both_signed(&l, &r)) {
                l.val.i %= r.val.i;
            } else {
                l.val.u %= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number eval_additive(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_multiplicative(list, &list);
    while (1) {
        switch (list->token) {
        case '+':
            r = eval_multiplicative(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i += r.val.i;
            } else {
                l.val.u += r.val.u;
            }
            break;
        case '-':
            r = eval_multiplicative(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i -= r.val.i;
            } else {
                l.val.u -= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number eval_shift(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_additive(list, &list);
    while (1) {
        switch (list->token) {
        case LSHIFT:
            r = eval_additive(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i <<= r.val.i;
            } else {
                l.val.u <<= r.val.u;
            }
            break;
        case RSHIFT:
            r = eval_additive(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i >>= r.val.i;
            } else {
                l.val.u >>= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number eval_relational(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_shift(list, &list);
    while (1) {
        switch (list->token) {
        case '<':
            r = eval_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i < r.val.i
                    : l.val.u < r.val.u;
            l.type = basic_type__long;
            break;
        case '>':
            r = eval_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i > r.val.i
                    : l.val.u > r.val.u;
            l.type = basic_type__long;
            break;
        case LEQ:
            r = eval_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i <= r.val.i
                    : l.val.u <= r.val.u;
            l.type = basic_type__long;
            break;
        case GEQ:
            r = eval_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i >= r.val.i
                    : l.val.u >= r.val.u;
            l.type = basic_type__long;
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number eval_equality(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_relational(list, &list);
    switch (list->token) {
    case EQ:
        r = eval_equality(list + 1, &list);
        l.val.i = both_signed(&l, &r)
                ? l.val.i == r.val.i
                : l.val.u == r.val.u;
        l.type = basic_type__long;
        break;
    case NEQ:
        r = eval_equality(list + 1, &list);
        l.val.i = both_signed(&l, &r)
                ? l.val.i != r.val.i
                : l.val.u != r.val.u;
        l.type = basic_type__long;
        break;
    default:
        break;
    }

    *endptr = list;
    return l;
}

static struct number eval_and(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_equality(list, &list);
    if (list->token == '&') {
        r = eval_and(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i &= r.val.i;
        } else {
            l.val.u &= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number eval_exclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_and(list, &list);
    if (list->token == '^') {
        r = eval_exclusive_or(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i ^= r.val.i;
        } else {
            l.val.u ^= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number eval_inclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_exclusive_or(list, &list);
    if (list->token == '|') {
        r = eval_exclusive_or(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i |= r.val.i;
        } else {
            l.val.u |= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number eval_logical_and(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_inclusive_or(list, &list);
    if (list->token == LOGICAL_AND) {
        r = eval_logical_and(list + 1, &list);
        l.val.i = l.val.u && r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number eval_logical_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = eval_logical_and(list, &list);
    if (list->token == LOGICAL_OR) {
        r = eval_logical_or(list + 1, &list);
        l.val.i = l.val.u || r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number expression(
    const struct token *list,
    const struct token **endptr)
{
    struct number a, b, c;

    a = eval_logical_or(list, &list);
    if (list->token == '?') {
        b = expression(list + 1, &list);
        expect(list, ':');
        c = expression(list + 1, &list);
        if (both_signed(&b, &c)) {
            a.val.i = a.val.u ? b.val.i : c.val.i;
            a.type = basic_type__long;
        } else {
            a.val.u = a.val.u ? b.val.u : c.val.u;
            a.type = basic_type__unsigned_long;
        }
    }

    *endptr = list;
    return a;
}

static void preprocess_include(const struct token line[])
{
    String path;

    if (line->token == STRING) {
        path = line->d.string;
        include_file(str_raw(path));
    } else if (line->token == '<') {
        line++;
        path = str_init("");
        while (line->token != NEWLINE) {
            if (line->token == '>') {
                break;
            }
            path = str_cat(path, tokstr(*line++));
        }
        if (!path.len || line->token != '>') {
            error("Invalid include directive.");
            exit(1);
        }
        include_system_file(str_raw(path));
    }
}

/* Function-like macro iff parenthesis immediately after identifier. */
static struct macro preprocess_define(
    const struct token *line,
    const struct token **endptr)
{
    struct macro macro = {0};
    struct token param = {PARAM}, t;
    TokenArray params = get_token_array();
    int i;

    t = *line++;
    if (!t.is_expandable) {
        error("Invalid definition of %s as a macro", str_raw(t.d.string));
        exit(1);
    }

    macro.name = t.d.string;
    macro.type = OBJECT_LIKE;
    macro.replacement = get_token_array();
    if (line->token == '(' && !line->leading_whitespace) {
        macro.type = FUNCTION_LIKE;
        line++;
        while (line->token != ')') {
            if (line->token == DOTS) {
                macro.is_vararg = 1;
                array_push_back(&params, ident__VA_ARGS__);
                line++;
                break;
            }
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
        param.d.val.i = -1;
        if (line->token == IDENTIFIER && macro.type == FUNCTION_LIKE) {
            for (i = 0; i < macro.params; ++i) {
                if (!tok_cmp(*line, array_get(&params, i))) {
                    param.d.val.i = i;
                    break;
                }
            }
        }
        if (param.d.val.i != -1) {
            array_push_back(&macro.replacement, param);
        } else {
            array_push_back(&macro.replacement, *line);
        }
        line++;
    }

    *endptr = line;
    release_token_array(params);
    return macro;
}

void preprocess_directive(TokenArray *array)
{
    struct number num;
    int def;
    enum state state;
    String s;
    const struct token *line = array->data;

    if (line->token == IF || !tok_cmp(*line, ident__elif)) {
        /*
         * Perform macro expansion only for if and elif directives,
         * before doing the expression parsing.
         */
        expand(array);
        line = array->data;
    }

    if (line->token == IF) {
        /*
         * Expressions are not necessarily valid in dead blocks, for
         * example can function-like macros be undefined.
         */
        if (in_active_block()) {
            num = expression(line + 1, &line);
            push(num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push(BRANCH_DISABLED);
        }
    } else if (line->token == ELSE) {
        state = pop();
        if (in_active_block()) {
            push(state == BRANCH_DEAD ? BRANCH_LIVE : BRANCH_DISABLED);
        } else {
            assert(state == BRANCH_DISABLED);
            push(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__elif)) {
        state = pop();
        if (in_active_block()) {
            if (state == BRANCH_DEAD) {
                num = expression(line + 1, &line);
                push(num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
            } else {
                push(BRANCH_DISABLED);
            }
        } else {
            assert(state == BRANCH_DISABLED);
            push(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__endif)) {
        pop();
    } else if (!tok_cmp(*line, ident__ifndef)) {
        if (in_active_block()) {
            line++;
            if (!line->is_expandable) {
                error("Expected identifier in 'ifndef' clause.");
                exit(1);
            }
            def = definition(line->d.string) == NULL;
            push(def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__ifdef)) {
        if (in_active_block()) {
            line++;
            if (!line->is_expandable) {
                error("Expected identifier in 'ifdef' clause.");
                exit(1);
            }
            def = definition(line->d.string) != NULL;
            push(def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push(BRANCH_DISABLED);
        }
    } else if (in_active_block()) {
        if (!tok_cmp(*line, ident__define)) {
            define(preprocess_define(line + 1, &line));
        } else if (!tok_cmp(*line, ident__undef)) {
            line++;
            if (!line->is_expandable) {
                error("Expected identifier in 'undef' clause.");
                exit(1);
            }
            undef(line->d.string);
        } else if (!tok_cmp(*line, ident__include)) {
            preprocess_include(line + 1);
        } else if (!tok_cmp(*line, ident__error)) {
            array->data++;
            array->length--;
            s = stringify(array).d.string;
            error("%s", str_raw(s));
            exit(1);
        } else {
            s = tokstr(*line);
            error("Unsupported preprocessor directive '%s'.", str_raw(s));
            exit(1);
        }
    }
}
