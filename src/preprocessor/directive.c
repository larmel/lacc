#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "directive.h"
#include "input.h"
#include "macro.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>

#define IDENT(s) {IDENTIFIER, 1, 0, 0, {0}, {SHORT_STRING_INIT(s)}}

INTERNAL struct token
    ident__include = IDENT("include"),
    ident__defined = IDENT("defined"),
    ident__define = IDENT("define"),
    ident__ifndef = IDENT("ifndef"),
    ident__ifdef = IDENT("ifdef"),
    ident__undef = IDENT("undef"),
    ident__elif = IDENT("elif"),
    ident__endif = IDENT("endif"),
    ident__error = IDENT("error"),
    ident__line = IDENT("line"),
    ident__pragma = IDENT("pragma"),
    ident__Pragma = IDENT("_" "Pragma"),
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

static void push_state(enum state c)
{
    array_push_back(&branch_stack, c);
}

static enum state pop_state(void)
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

INTERNAL int in_active_block(void)
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
        a = basic_token[token].d.string;
        b = list->d.string;
        error("Expected '%s', but got '%s'.", str_raw(a), str_raw(b));
        exit(1);
    }
}

static struct number preprocess_expression(
    const struct token *list,
    const struct token **endptr);

/*
 * Macro expansions should already have been done. Stray identifiers are
 * interpreted as zero constants.
 *
 * Normalize all values to be of type long or unsigned long. Floating
 * point numbers are not permitted by the standard.
 */
static struct number preprocess_primary(
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
    case PREP_CHAR:
        n = convert_preprocessing_char(*list);
        assert(n.token == NUMBER);
        num.type = n.type;
        num.val = n.d.val;
        break;
    case '(':
        num = preprocess_expression(list + 1, &list);
        expect(list, ')');
        break;
    default:
        if (!list->is_expandable) {
            s = list->d.string;
            error("Invalid primary expression '%s' in directive.", str_raw(s));
            exit(1);
        }
    case IDENTIFIER:
        assert(!macro_definition(list->d.string));
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

static struct number preprocess_unary(
    const struct token *list,
    const struct token **endptr)
{
    struct number r;

    switch (list->token) {
    case '+':
        r = preprocess_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = + r.val.i;
        }
        return r;
    case '-':
        r = preprocess_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = - r.val.i;
        } else {
            r.val.u = - r.val.u;
        }
        return r;
    case '~':
        r = preprocess_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ~ r.val.i;
        } else {
            r.val.u = ~ r.val.u;
        }
        return r;
    case '!':
        r = preprocess_unary(list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ! r.val.i;
        } else {
            r.val.u = ! r.val.u;
        }
        return r;
    default:
        return preprocess_primary(list, endptr);
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

static struct number preprocess_multiplicative(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_unary(list, &list);
    while (1) {
        switch (list->token) {
        case '*':
            r = preprocess_unary(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i *= r.val.i;
            } else {
                l.val.u *= r.val.u;
            }
            break;
        case '/':
            r = preprocess_unary(list + 1, &list);
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
            r = preprocess_unary(list + 1, &list);
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

static struct number preprocess_additive(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_multiplicative(list, &list);
    while (1) {
        switch (list->token) {
        case '+':
            r = preprocess_multiplicative(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i += r.val.i;
            } else {
                l.val.u += r.val.u;
            }
            break;
        case '-':
            r = preprocess_multiplicative(list + 1, &list);
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

static struct number preprocess_shift(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_additive(list, &list);
    while (1) {
        switch (list->token) {
        case LSHIFT:
            r = preprocess_additive(list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i <<= r.val.i;
            } else {
                l.val.u <<= r.val.u;
            }
            break;
        case RSHIFT:
            r = preprocess_additive(list + 1, &list);
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

static struct number preprocess_relational(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_shift(list, &list);
    while (1) {
        switch (list->token) {
        case '<':
            r = preprocess_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i < r.val.i
                    : l.val.u < r.val.u;
            l.type = basic_type__long;
            break;
        case '>':
            r = preprocess_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i > r.val.i
                    : l.val.u > r.val.u;
            l.type = basic_type__long;
            break;
        case LEQ:
            r = preprocess_shift(list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i <= r.val.i
                    : l.val.u <= r.val.u;
            l.type = basic_type__long;
            break;
        case GEQ:
            r = preprocess_shift(list + 1, &list);
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

static struct number preprocess_equality(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_relational(list, &list);
    switch (list->token) {
    case EQ:
        r = preprocess_equality(list + 1, &list);
        l.val.i = both_signed(&l, &r)
                ? l.val.i == r.val.i
                : l.val.u == r.val.u;
        l.type = basic_type__long;
        break;
    case NEQ:
        r = preprocess_equality(list + 1, &list);
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

static struct number preprocess_and(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_equality(list, &list);
    if (list->token == '&') {
        r = preprocess_and(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i &= r.val.i;
        } else {
            l.val.u &= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_exclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_and(list, &list);
    if (list->token == '^') {
        r = preprocess_exclusive_or(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i ^= r.val.i;
        } else {
            l.val.u ^= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_inclusive_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_exclusive_or(list, &list);
    if (list->token == '|') {
        r = preprocess_exclusive_or(list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i |= r.val.i;
        } else {
            l.val.u |= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_logical_and(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_inclusive_or(list, &list);
    if (list->token == LOGICAL_AND) {
        r = preprocess_logical_and(list + 1, &list);
        l.val.i = l.val.u && r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number preprocess_logical_or(
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_logical_and(list, &list);
    if (list->token == LOGICAL_OR) {
        r = preprocess_logical_or(list + 1, &list);
        l.val.i = l.val.u || r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number preprocess_conditional(
    const struct token *list,
    const struct token **endptr)
{
    struct number a, b, c;

    a = preprocess_logical_or(list, &list);
    if (list->token == '?') {
        b = preprocess_expression(list + 1, &list);
        expect(list, ':');
        c = preprocess_conditional(list + 1, &list);
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

static struct number preprocess_expression(
    const struct token *list,
    const struct token **endptr)
{
    struct number n;

    do {
        n = preprocess_conditional(list, &list);
    } while (list->token == ',' && list++);

    *endptr = list;
    return n;
}

static struct number preprocess_constant_expression(
    const struct token *list,
    const struct token **endptr)
{
    return preprocess_conditional(list, endptr);
}

/*
 * Preprocess include directive, which should have any of the following
 * forms:
 *
 *     #include "foo.h"
 *     #include <foo.h>
 *     #include FOO
 *
 * Macro expansion is performed if neither of the first two forms match.
 */
static void preprocess_include(TokenArray *line)
{
    int i, len, exp;
    String path;
    struct token t;

    assert(!tok_cmp(array_get(line, 0), ident__include));
    assert(array_back(line).token == NEWLINE);

    array_erase(line, 0);
    for (exp = 0; exp < 2; ++exp) {
        len = array_len(line);
        t = array_get(line, 0);
        if (t.token == PREP_STRING) {
            if (len > 2) {
                error("Stray tokens in include directive.");
                exit(1);
            }
            path = t.d.string;
            include_file(str_raw(path));
            break;
        }
        if (t.token == '<' && array_get(line, len - 2).token == '>') {
            path = str_empty();
            for (i = 1; i < len - 2; ++i) {
                t = array_get(line, i);
                path = str_cat(path, t.d.string);
            }
            include_system_file(str_raw(path));
            break;
        }
        if (!exp) {
            expand(line);
        } else {
            error("Invalid include directive.");
            exit(1);
        }
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
            if (!line->is_expandable) {
                error("Invalid macro parameter, expected identifer.");
                exit(1);
            }
            array_push_back(&params, *line++);
            if (line->token == DOTS) {
                macro.is_vararg = 1;
                line++;
                break;
            } else if (line->token != ',')
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
        if (line->is_expandable && macro.type == FUNCTION_LIKE) {
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

/*
 * Handle #line directive, which is in one of the following forms:
 *
 *     #line 42
 *     #line 42 "foo.c"
 *
 * Update line number, and optionally name, of file being processed.
 */
static void preprocess_line_directive(const struct token *line)
{
    struct token t;

    t = *line++;
    if (t.token == PREP_NUMBER) {
        t = convert_preprocessing_number(t);
    }

    if (t.token != NUMBER || !is_int(t.type) || t.d.val.i <= 0) {
        error("Expected positive integer in #line directive.");
        exit(1);
    }

    current_file_line = t.d.val.i - 1;
    if (line->token == PREP_STRING) {
        current_file_path = line->d.string;
        line++;
    }

    if (line->token != NEWLINE) {
        error("Unexpected token in #line directive.");
        exit(1);
    }
}

INTERNAL void preprocess_directive(TokenArray *array)
{
    struct number num;
    int def;
    enum state state;
    String s;
    const struct token *line = array->data;

    /*
     * Perform macro expansion only for if, elif and line directives,
     * before doing any expression parsing.
     */
    if (line->token == IF
        || !tok_cmp(*line, ident__elif)
        || (in_active_block() && !tok_cmp(*line, ident__line)))
    {
        expand(array);
        line = array->data;
    }

    if (line->token == IF) {
        /*
         * Expressions are not necessarily valid in dead blocks, for
         * example can function-like macros be undefined.
         */
        if (in_active_block()) {
            num = preprocess_constant_expression(line + 1, &line);
            push_state(num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(BRANCH_DISABLED);
        }
    } else if (line->token == ELSE) {
        state = pop_state();
        if (in_active_block()) {
            push_state(state == BRANCH_DEAD ? BRANCH_LIVE : BRANCH_DISABLED);
        } else {
            assert(state == BRANCH_DISABLED);
            push_state(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__elif)) {
        state = pop_state();
        if (in_active_block()) {
            if (state == BRANCH_DEAD) {
                num = preprocess_constant_expression(line + 1, &line);
                push_state(num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
            } else {
                push_state(BRANCH_DISABLED);
            }
        } else {
            assert(state == BRANCH_DISABLED);
            push_state(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__endif)) {
        pop_state();
    } else if (!tok_cmp(*line, ident__ifndef)) {
        if (in_active_block()) {
            line++;
            if (!line->is_expandable) {
                error("Expected identifier in 'ifndef' clause.");
                exit(1);
            }
            def = macro_definition(line->d.string) == NULL;
            push_state(def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__ifdef)) {
        if (in_active_block()) {
            line++;
            if (!line->is_expandable) {
                error("Expected identifier in 'ifdef' clause.");
                exit(1);
            }
            def = macro_definition(line->d.string) != NULL;
            push_state(def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(BRANCH_DISABLED);
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
            preprocess_include(array);
        } else if (!tok_cmp(*line, ident__line)) {
            preprocess_line_directive(line + 1);
        } else if (!tok_cmp(*line, ident__error)) {
            array->data++;
            array->length--;
            s = stringify(array).d.string;
            error("%s", str_raw(s));
            exit(1);
        } else {
            s = line->d.string;
            error("Unsupported preprocessor directive '%s'.", str_raw(s));
            exit(1);
        }
    }
}
