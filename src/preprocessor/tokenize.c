#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "strtab.h"
#include "tokenize.h"
#include <lacc/context.h>
#include <lacc/type.h>

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Static initializer for token. Only works with string representation
 * that can fit inline.
 */
#define TOK(t, s) {(t), 0, 0, 0, {0}, {SHORT_STRING_INIT(s)}}
#define IDN(t, s) {(t), 1, 0, 0, {0}, {SHORT_STRING_INIT(s)}}

INTERNAL const struct token basic_token[] = {
/* 0x00 */  TOK(END, "$"),              IDN(AUTO, "auto"),
            IDN(BREAK, "break"),        IDN(CASE, "case"),
            IDN(CHAR, "char"),          IDN(CONST, "const"),
            IDN(CONTINUE, "continue"),  IDN(DEFAULT, "default"),
/* 0x08 */  IDN(DO, "do"),              IDN(DOUBLE, "double"),
            TOK(NEWLINE, "\n"),         IDN(ELSE, "else"),
            IDN(ENUM, "enum"),          IDN(EXTERN, "extern"),
            IDN(FLOAT, "float"),        IDN(FOR, "for"), 
/* 0x10 */  IDN(GOTO, "goto"),          IDN(IF, "if"),
            IDN(INT, "int"),            IDN(LONG, "long"),
            IDN(REGISTER, "register"),  IDN(RETURN, "return"),
            IDN(SHORT, "short"),        IDN(SIGNED, "signed"),
/* 0x18 */  IDN(SIZEOF, "sizeof"),      IDN(STATIC, "static"),
            IDN(STRUCT, "struct"),      IDN(SWITCH, "switch"),
            IDN(TYPEDEF, "typedef"),    IDN(UNION, "union"),
            IDN(UNSIGNED, "unsigned"),  IDN(VOID, "void"),
/* 0x20 */  IDN(INLINE, "inline"),      TOK(NOT, "!"),
            IDN(VOLATILE, "volatile"),  TOK(HASH, "#"),
            TOK(DOLLAR, "$"),           TOK(MODULO, "%"),
            TOK(AND, "&"),              IDN(WHILE, "while"),
/* 0x28 */  TOK(OPEN_PAREN, "("),       TOK(CLOSE_PAREN, ")"),
            TOK(STAR, "*"),             TOK(PLUS, "+"),
            TOK(COMMA, ","),            TOK(MINUS, "-"),
            TOK(DOT, "."),              TOK(SLASH, "/"),
/* 0x30 */  IDN(RESTRICT, "restrict"),  TOK(ALIGNOF, "_Alignof"),
            TOK(BOOL, "_Bool"),         IDN(NORETURN, "_Noreturn"),
            {0},                        {0},
            {0},                        {0},
/* 0x38 */  IDN(STATIC_ASSERT, "_Static_assert"),     {0},
            TOK(COLON, ":"),            TOK(SEMICOLON, ";"),
            TOK(LT, "<"),               TOK(ASSIGN, "="),
            TOK(GT, ">"),               TOK(QUESTION, "?"),
/* 0x40 */  TOK(AMPERSAND, "@"),        TOK(LOGICAL_OR, "||"),
            TOK(LOGICAL_AND, "&&"),     TOK(LEQ, "<="),
            TOK(GEQ, ">="),             TOK(EQ, "=="),
            TOK(NEQ, "!="),             TOK(ARROW, "->"),
/* 0x48 */  TOK(INCREMENT, "++"),       TOK(DECREMENT, "--"),
            TOK(LSHIFT, "<<"),          TOK(RSHIFT, ">>"),
            TOK(MUL_ASSIGN, "*="),      TOK(DIV_ASSIGN, "/="),
            TOK(MOD_ASSIGN, "%="),      TOK(PLUS_ASSIGN, "+="),
/* 0x50 */  TOK(MINUS_ASSIGN, "-="),    TOK(LSHIFT_ASSIGN, "<<="),
            TOK(RSHIFT_ASSIGN, ">>="),  TOK(AND_ASSIGN, "&="),
            TOK(XOR_ASSIGN, "^="),      TOK(OR_ASSIGN, "|="),
            TOK(TOKEN_PASTE, "##"),     TOK(DOTS, "..."),
/* 0x58 */  {0},                        {0},
            {0},                        TOK(OPEN_BRACKET, "["),
            TOK(BACKSLASH, "\\"),       TOK(CLOSE_BRACKET, "]"),
            TOK(XOR, "^"),              {0},
/* 0x60 */  TOK(BACKTICK, "`"),         {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x68 */  TOK(ASM, "__asm__"),        {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x70 */  {0},                        {0},
            {0},                        {0},
            {NUMBER},                   {IDENTIFIER, 1},
            {STRING},                   {PARAM},
/* 0x78 */  {PREP_NUMBER},              {PREP_CHAR},
            {PREP_STRING},              TOK(OPEN_CURLY, "{"),
            TOK(OR, "|"),               TOK(CLOSE_CURLY, "}"),
            TOK(NEG, "~"),              {0},
};

/*
 * Parse preprocessing number, which starts with an optional period
 * before a digit, then a sequence of period, letter underscore, digit,
 * or any of 'e+', 'e-', 'E+', 'E-'.
 *
 * This represents a superset of valid numbers in C, but is required
 * as intermediate representation for preprocessing.
 *
 * There is no such thing as a negative literal; expressions like '-2'
 * is the unary operator applied to the number 2.
 *
 * Regular expression:
 *
 *      (\.)?(0-9){\.a-zA-Z_0-9(e+|e-|E+|E-)}*
 *
 */
static struct token stringtonum(const char *in, const char **endptr)
{
    const char *ptr = in;
    struct token tok = {PREP_NUMBER};

    if (*in == '.') {
        in++;
    }

    assert(isdigit(*in));
    while (1) {
        if (isdigit(*in) || *in == '.' || *in == '_') {
            in++;
        } else if (isalpha(*in)) {
            if ((tolower(*in) == 'e' ||
                    (context.standard >= STD_C99 && tolower(*in) == 'p'))
                && (in[1] == '+' || in[1] == '-'))
            {
                in++;
            }
            in++;
        } else {
            break;
        }
    }

    tok.d.string = str_intern(ptr, in - ptr);
    *endptr = in;
    return tok;
}

enum suffix {
    SUFFIX_NONE = 0,
    SUFFIX_U = 0x1,
    SUFFIX_L = 0x2,
    SUFFIX_UL = SUFFIX_U | SUFFIX_L,
    SUFFIX_LL = (SUFFIX_L << 1) | SUFFIX_L,
    SUFFIX_ULL = SUFFIX_U | SUFFIX_LL
};

static enum suffix read_integer_suffix(const char *ptr, const char **endptr)
{
    enum suffix s = SUFFIX_NONE;

    if (tolower(*ptr) == 'u') {
        s = SUFFIX_U;
        ptr++;
    }

    if (tolower(*ptr) == 'l') {
        s |= SUFFIX_L;
        ptr++;
        if (*ptr == ptr[-1]) {
            s |= SUFFIX_LL;
            ptr++;
        }

        if (!(s & SUFFIX_U) && tolower(*ptr) == 'u') {
            s |= SUFFIX_U;
            ptr++;
        }
    }

    *endptr = ptr;
    return s;
}

static const Type constant_integer_type(
    unsigned long int value,
    enum suffix suffix,
    int is_decimal)
{
    Type type;

    switch (suffix) {
    case SUFFIX_NONE:
        if (value <= INT_MAX) {
            type = basic_type__int;
        } else if (!is_decimal && value <= UINT_MAX) {
            type = basic_type__unsigned_int;
        } else if (value <= LONG_MAX) {
            type = basic_type__long;
        } else {
            type = basic_type__unsigned_long;
            if (is_decimal) {
                warning("Conversion of decimal constant to unsigned.");
            }
        }
        break;
    case SUFFIX_U:
        if (value <= UINT_MAX) {
            type = basic_type__unsigned_int;
        } else {
            type = basic_type__unsigned_long;
        }
        break;
    case SUFFIX_L:
    case SUFFIX_LL:
        if (value <= LONG_MAX) {
            type = basic_type__long;
        } else {
            type = basic_type__unsigned_long;
            if (is_decimal) {
                warning("Conversion of decimal constant to unsigned.");
            }
        }
        break;
    case SUFFIX_UL:
    case SUFFIX_ULL:
        type = basic_type__unsigned_long;
        break;
    }

    return type;
}

INTERNAL struct token convert_preprocessing_number(struct token t)
{
    const char *str;
    const char *endptr;
    int len;
    enum suffix suffix;
    struct token tok = {NUMBER};

    assert(t.token == PREP_NUMBER);
    str = str_raw(t.d.string);
    len = str_len(t.d.string);
    tok.leading_whitespace = t.leading_whitespace;

    /*
     * Try to read as integer. Handle suffixes u, l, ll, ul, ull, in all
     * permuations of upper- and lower case.
     */
    errno = 0;
    tok.d.val.u = strtoul(str, (char **) &endptr, 0);
    suffix = read_integer_suffix(endptr, &endptr);
    if (endptr - str == len) {
        assert(isdigit(*str));
        tok.type = constant_integer_type(tok.d.val.u, suffix, *str != '0');
    } else {
        /*
         * If the integer conversion did not consume the whole token,
         * try to read as floating point number.
         *
         * Note: not using strtold for long double conversion, so might
         * get incorrect results compared to other compilers.
         */
        errno = 0;
        tok.type = basic_type__double;
        tok.d.val.d = strtod(str, (char **) &endptr);
        if (endptr - str < len) {
            if (*endptr == 'f' || *endptr == 'F') {
                tok.type = basic_type__float;
                tok.d.val.f = (float) tok.d.val.d;
                endptr++;
            } else if (*endptr == 'l' || *endptr == 'L') {
                tok.type = basic_type__long_double;
                tok.d.val = put_long_double((long double) tok.d.val.d);
                endptr++;
            }
        }
    }

    if (errno || (endptr - str != len)) {
        if (errno == ERANGE) {
            error("Numeric literal '%s' is out of range.", str);
        } else {
            error("Invalid numeric literal '%s'.", str);
        }
        exit(1);
    }

    return tok;
}

#define isoctal(c) ((c) >= '0' && (c) < '8')

static char convert_escape_sequence(const char *in, const char **endptr)
{
    static char buf[4];
    long n;
    int i;

    *endptr = in + 1;
    switch (*in) {
    case 'a': return '\a';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    case '?': return '\?';
    case '\'': return '\'';
    case '\"': return '\"';
    case '\\': return '\\';
    case 'x':
        if (!isxdigit(in[1])) {
            error("Empty hexadecimal escape sequence.");
            exit(1);
        }
        return (char) strtol(&in[1], (char **) endptr, 16);
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
        for (i = 0; i < 3 && isoctal(in[i]); ++i) {
            buf[i] = in[i];
        }
        buf[i] = '\0';
        n = strtol(buf, (char **) endptr, 8);
        *endptr = in + i;
        return (char) n;
    default:
        error("Invalid escape sequence '\\%c'.", *in);
        exit(1);
    }
}

static char convert_char(const char *in, const char **endptr)
{
    char c;

    if (*in == '\\') {
        c = convert_escape_sequence(in + 1, endptr);
    } else {
        c = *in;
        *endptr = in + 1;
    }

    return c;
}

static char *string_buffer;
static size_t string_buffer_cap;

INTERNAL void tokenize_reset(void)
{
    if (string_buffer) {
        free(string_buffer);
        string_buffer = NULL;
        string_buffer_cap = 0;
    }
}

static char *get_string_buffer(size_t length)
{
    if (length > string_buffer_cap) {
        string_buffer_cap = length;
        string_buffer = realloc(string_buffer, length);
    }

    return string_buffer;
}

INTERNAL struct token convert_preprocessing_string(struct token t)
{
    struct token tok = {STRING};
    const char *raw, *ptr;
    char *buf, *btr;
    size_t len;

    raw = str_raw(t.d.string);
    len = str_len(t.d.string);
    buf = get_string_buffer(len);
    btr = buf;
    ptr = raw;
    while (ptr - raw < len) {
        *btr++ = convert_char(ptr, &ptr);
    }

    tok.d.string = str_intern(buf, btr - buf);
    return tok;
}

INTERNAL struct token convert_preprocessing_char(struct token t)
{
    struct token tok = {NUMBER};
    const char *raw;

    raw = str_raw(t.d.string);
    tok.type = basic_type__int;
    tok.d.val.i = convert_char(raw, &raw);
    return tok;
}

/*
 * Parse character escape sequences like '\xaf', '\0', '\077' etc,
 * starting from *in.
 *
 * Input is not validated here, this must be delayed until after
 * preprocessing when converting PREP_STRING and PREP_CHAR.
 */
static void parse_escape_sequence(const char *in, const char **endptr)
{
    switch (*in++) {
    case 'x':
        while (isxdigit(*in)) {
            in++;
        }
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
        if (isoctal(*in)) {
            in++;
            if (isoctal(*in)) {
                in++;
            }
        }
        break;
    default:
        break;
    }

    *endptr = in;
}

/*
 * Parse character literals in the format 'a', '\xaf', '\0', '\077' etc,
 * starting from *in. The position of the character after the last '
 * character is stored in endptr.
 */
static struct token strtochar(const char *in, const char **endptr)
{
    struct token tok = {PREP_CHAR};
    const char *start;

    assert(*in == '\'');
    start = ++in;
    if (*in == '\\') {
        parse_escape_sequence(in + 1, &in);
    } else if (*in != '\'') {
        in++;
    } else {
        error("Empty character constant.");
        exit(1);
    }

    if (*in != '\'') {
        error("Multi-character constants are not supported.");
        exit(1);
    }

    tok.d.string = str_intern(start, in - start);
    *endptr = in + 1;
    return tok;
}

/* Parse string literal inputs delimited by quotation marks. */
static struct token strtostr(const char *in, const char **endptr)
{
    struct token tok = {PREP_STRING};
    const char *start;

    assert(*in == '"');
    start = ++in;
    *endptr = in;

    while (*in != '"') {
        if (*in == '\\') {
            parse_escape_sequence(in + 1, &in);
        } else {
            in++;
        }
    }

    assert(*in == '"');
    tok.d.string = str_intern(start, in - start);
    *endptr = in + 1;
    return tok;
}

/*
 * Valid identifier character, except in the first position which does
 * not allow numbers.
 */
#define isident(c) (isalnum(c) || (c) == '_')

/*
 * Macros to make state state machine implementation of identifier and
 * operator tokenization simpler.
 */
#define E(i) !isident(in[i])
#define M(i, c) (in[i] == (c))

#define M1(a) (M(0, a))
#define M2(a, b) (M(0, a) && M(1, b))
#define M3(a, b, c) (M2(a, b) && M(2, c))
#define M4(a, b, c, d) (M3(a, b, c) && M(3, d))
#define M5(a, b, c, d, e) (M4(a, b, c, d) && M(4, e))
#define M6(a, b, c, d, e, f) (M5(a, b, c, d, e) && M(5, f))
#define M7(a, b, c, d, e, f, g) (M6(a, b, c, d, e, f) && M(6, g))

#define S1(a) (M1(a) && E(1))
#define S2(a, b) (M2(a, b) && E(2))
#define S3(a, b, c) (M3(a, b, c) && E(3))
#define S4(a, b, c, d) (M4(a, b, c, d) && E(4))
#define S5(a, b, c, d, e) (M5(a, b, c, d, e) && E(5))
#define S6(a, b, c, d, e, f) (M6(a, b, c, d, e, f) && E(6))
#define S7(a, b, c, d, e, f, g) (M7(a, b, c, d, e, f, g) && E(7))

#define MATCH(id, len) \
    do { \
        *endptr = start + len; \
        return basic_token[id]; \
    } while (0)

/*
 * Parse string as keyword or identifier. First character should be
 * alphabetic or underscore.
 */
static struct token strtoident(const char *in, const char **endptr)
{
    const char *start = in;
    struct token ident = {IDENTIFIER};

    switch (*in++) {
    case '_':
        if (S6('_', 'a', 's', 'm', '_', '_')) MATCH(ASM, 7);
        if (S4('B', 'o', 'o', 'l')) MATCH(BOOL, 5);
        if (S7('A', 'l', 'i', 'g', 'n', 'o', 'f')) MATCH(ALIGNOF, 8);
        if (!strncmp(in, "Noreturn", 8) && E(8)) MATCH(NORETURN, 9);
        if (!strncmp(in, "Static_assert", 13) && E(13))
            MATCH(STATIC_ASSERT, 14);
        break;
    case 'a':
        if (S3('u', 't', 'o')) MATCH(AUTO, 4);
        break;
    case 'b':
        if (S4('r', 'e', 'a', 'k')) MATCH(BREAK, 5);
        break;
    case 'c':
        if (S3('a', 's', 'e')) MATCH(CASE, 4);
        if (S3('h', 'a', 'r')) MATCH(CHAR, 4);
        if (M2('o', 'n')) {
            in += 2;
            if (S2('s', 't')) MATCH(CONST, 5);
            if (S5('t', 'i', 'n', 'u', 'e')) MATCH(CONTINUE, 8);
        }
        break;
    case 'd':
        if (S6('e', 'f', 'a', 'u', 'l', 't')) MATCH(DEFAULT, 7);
        if (*in++ == 'o') {
            if (S4('u', 'b', 'l', 'e')) MATCH(DOUBLE, 6);
            if (E(0)) MATCH(DO, 2);
        }
        break;
    case 'e':
        if (S3('l', 's', 'e')) MATCH(ELSE, 4);
        if (S3('n', 'u', 'm')) MATCH(ENUM, 4);
        if (S5('x', 't', 'e', 'r', 'n')) MATCH(EXTERN, 6);
        break;
    case 'f':
        if (S4('l', 'o', 'a', 't')) MATCH(FLOAT, 5);
        if (S2('o', 'r')) MATCH(FOR, 3);
        break;
    case 'g':
        if (S3('o', 't', 'o')) MATCH(GOTO, 4);
        break;
    case 'i':
        if (S1('f')) MATCH(IF, 2);
        if (*in++ == 'n') {
            if (S1('t')) MATCH(INT, 3);
            if (context.standard >= STD_C99) {
                if (S4('l', 'i', 'n', 'e')) MATCH(INLINE, 6);
            }
        }
        break;
    case 'l':
        if (S3('o', 'n', 'g')) MATCH(LONG, 4);
        break;
    case 'r':
        if (*in++ == 'e') {
            if (S6('g', 'i', 's', 't', 'e', 'r')) MATCH(REGISTER, 8);
            if (S4('t', 'u', 'r', 'n')) MATCH(RETURN, 6);
            if (context.standard >= STD_C99) {
                if (S6('s', 't', 'r', 'i', 'c', 't')) MATCH(RESTRICT, 8);
            }
        }
        break;
    case 's':
        if (S4('h', 'o', 'r', 't')) MATCH(SHORT, 5);
        if (S5('w', 'i', 't', 'c', 'h')) MATCH(SWITCH, 6);
        switch (*in++) {
        case 'i':
            if (S4('g', 'n', 'e', 'd')) MATCH(SIGNED, 6);
            if (S4('z', 'e', 'o', 'f')) MATCH(SIZEOF, 6);
            break;
        case 't':
            if (S4('a', 't', 'i', 'c')) MATCH(STATIC, 6);
            if (S4('r', 'u', 'c', 't')) MATCH(STRUCT, 6);
            break;
        default: break;
        }
        break;
    case 't':
        if (S6('y', 'p', 'e', 'd', 'e', 'f')) MATCH(TYPEDEF, 7);
        break;
    case 'u':
        if (*in++ == 'n') {
            if (S3('i', 'o', 'n')) MATCH(UNION, 5);
            if (S6('s', 'i', 'g', 'n', 'e', 'd')) MATCH(UNSIGNED, 8);
        }
        break;
    case 'v':
        if (*in++ == 'o') {
            if (S2('i', 'd')) MATCH(VOID, 4);
            if (S6('l', 'a', 't', 'i', 'l', 'e')) MATCH(VOLATILE, 8);
        }
        break;
    case 'w':
        if (S4('h', 'i', 'l', 'e')) MATCH(WHILE, 5);
    default:
        break;
    }

    in--;
    while (isident(*in)) {
        in++;
    }

    ident.d.string = str_intern(start, in - start);
    ident.is_expandable = 1;
    *endptr = in;
    return ident;
}

static struct token strtoop(const char *in, const char **endptr)
{
    const char *start = in;
    struct token t;

    switch (*in++) {
    case '*':
        if (*in == '=') MATCH(MUL_ASSIGN, 2);
        break;
    case '/':
        if (*in == '=') MATCH(DIV_ASSIGN, 2);
        break;
    case '%':
        if (*in == '=') MATCH(MOD_ASSIGN, 2);
        break;
    case '+':
        if (*in == '+') MATCH(INCREMENT, 2);
        if (*in == '=') MATCH(PLUS_ASSIGN, 2);
        break;
    case '-':
        if (*in == '>') MATCH(ARROW, 2);
        if (*in == '-') MATCH(DECREMENT, 2);
        if (*in == '=') MATCH(MINUS_ASSIGN, 2);
        break;
    case '<':
        if (*in == '=') MATCH(LEQ, 2);
        if (*in++ == '<') {
            if (*in == '=') MATCH(LSHIFT_ASSIGN, 3);
            MATCH(LSHIFT, 2);
        }
        break;
    case '>':
        if (*in == '=') MATCH(GEQ, 2);
        if (*in++ == '>') {
            if (*in == '=') MATCH(RSHIFT_ASSIGN, 3);
            MATCH(RSHIFT, 2);
        }
        break;
    case '&':
        if (*in == '=') MATCH(AND_ASSIGN, 2);
        if (*in == '&') MATCH(LOGICAL_AND, 2);
        break;
    case '^':
        if (*in == '=') MATCH(XOR_ASSIGN, 2);
        break;
    case '|':
        if (*in == '=') MATCH(OR_ASSIGN, 2);
        if (*in == '|') MATCH(LOGICAL_OR, 2);
        break;
    case '.':
        if (*in++ == '.' && *in == '.') MATCH(DOTS, 3);
        break;
    case '=':
        if (*in == '=') MATCH(EQ, 2);
        break;
    case '!':
        if (*in == '=') MATCH(NEQ, 2);
        break;
    case '#':
        if (*in == '#') MATCH(TOKEN_PASTE, 2);
        break;
    default:
        break;
    }

    *endptr = start + 1;
    t = basic_token[(int) *start];
    if (t.token == END) {
        error("Unknown token '%c'", (int) *start);
        exit(1);
    }

    return t;
}

static int skip_spaces(const char *in, const char **endptr)
{
    const char *start = in;

    while (isspace(*in))
        in++;

    *endptr = in;
    return in - start;
}

INTERNAL struct token tokenize(const char *in, const char **endptr)
{
    int ws;
    struct token tok;

    assert(in);
    assert(endptr);

    ws = skip_spaces(in, endptr);
    in = *endptr;

    if (isalpha(*in) || *in == '_') {
        tok = strtoident(in, endptr);
    } else if (*in == '\0') {
        tok = basic_token[END];
    } else if (isdigit(*in) || (*in == '.' && isdigit(in[1]))) {
        tok = stringtonum(in, endptr);
    } else if (*in == '"') {
        tok = strtostr(in, endptr);
    } else if (*in == '\'') {
        tok = strtochar(in, endptr);
    } else {
        tok = strtoop(in, endptr);
    }

    tok.leading_whitespace = ws;
    return tok;
}
