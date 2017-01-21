#define _XOPEN_SOURCE 500 /* snprintf */
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
#define TOK(t, s) {(t), 0, 0, 0, 0, {0}, {SHORT_STRING_INIT(s)}}
#define IDN(t, s) {(t), 0, 1, 0, 0, {0}, {SHORT_STRING_INIT(s)}}

const struct token basic_token[] = {
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
            IDN(WHILE, "while"),        TOK(MODULO, "%"),
            TOK(AND, "&"),              {0},
/* 0x28 */  TOK(OPEN_PAREN, "("),       TOK(CLOSE_PAREN, ")"),
            TOK(STAR, "*"),             TOK(PLUS, "+"),
            TOK(COMMA, ","),            TOK(MINUS, "-"),
            TOK(DOT, "."),              TOK(SLASH, "/"),
/* 0x30 */  {0},                        {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x38 */  {0},                        {0},
            TOK(COLON, ":"),            TOK(SEMICOLON, ";"),
            TOK(LT, "<"),               TOK(ASSIGN, "="),
            TOK(GT, ">"),               TOK(QUESTION, "?"),
/* 0x40 */  TOK(DOTS, "..."),           TOK(LOGICAL_OR, "||"),
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
            TOK(TOKEN_PASTE, "##"),     {0},
/* 0x58 */  {0},                        {0},
            {0},                        TOK(OPEN_BRACKET, "["),
            {0},                        TOK(CLOSE_BRACKET, "]"),
            TOK(XOR, "^"),              {0},
/* 0x60 */  {0},                        {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x68 */  {0},                        {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x70 */  {0},                        {0},
            {0},                        {0},
            {NUMBER},                   {IDENTIFIER, 1},
            {STRING},                   {PARAM},
/* 0x78 */  {EMPTY_ARG},                {PREP_NUMBER},
            {0},                        TOK(OPEN_CURLY, "{"),
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
static struct token strtonum(char *in, char **endptr)
{
    char *ptr = in;
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

    tok.d.string = str_register(ptr, in - ptr);
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

static enum suffix read_integer_suffix(char *ptr, char **endptr)
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

struct token convert_preprocessing_number(struct token t)
{
    const char *str;
    char *endptr;
    int len;
    enum suffix suffix;
    struct token tok = {NUMBER};

    assert(t.token == PREP_NUMBER);
    str = str_raw(t.d.string);
    len = t.d.string.len;
    tok.leading_whitespace = t.leading_whitespace;

    /*
     * Try to read as integer. Handle suffixes u, l, ll, ul, ull, in all
     * permuations of upper- and lower case.
     */
    errno = 0;
    tok.d.val.u = strtoul(str, &endptr, 0);
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
        tok.d.val.d = strtod(str, &endptr);
        if (endptr - str < len) {
            if (*endptr == 'f' || *endptr == 'F') {
                tok.type = basic_type__float;
                tok.d.val.f = (float) tok.d.val.d;
                endptr++;
            } else if (*endptr == 'l' || *endptr == 'L') {
                tok.type = basic_type__long_double;
                tok.d.val.ld = (long double) tok.d.val.d;
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

/*
 * Parse character escape code, including octal and hexadecimal number
 * literals. Unescaped characters are returned as-is. Invalid escape
 * sequences continues with an error, consuming only the backslash.
 */
static char escpchar(char *in, char **endptr)
{
    static char buf[4];
    long n;
    int i;

    if (*in == '\\') {
        *endptr = in + 2;
        switch (in[1]) {
        case 'a': return 0x7;
        case 'b': return 0x8;
        case 't': return 0x9;
        case 'n': return 0xa;
        case 'v': return 0xb;
        case 'f': return 0xc;
        case 'r': return 0xd;
        case '\\': return '\\';
        case '?': return '\?';
        case '\'': return '\'';
        case '\"': return '\"';
        case 'x':
            return (char) strtol(&in[2], endptr, 16);
        case '0':
            if (!isoctal(in[2])) {
                return '\0';
            }
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
            buf[0] = in[1];
            for (i = 1; i < 3 && isoctal(in[1 + i]); ++i) {
                buf[i] = in[1 + i];
            }
            buf[i] = '\0';
            n = strtol(buf, endptr, 8);
            *endptr = in + i + 1;
            return (char) n;
        default:
            error("Invalid escape sequence '\\%c'.", in[1]);
            break;
        }
    }

    *endptr = in + 1;
    return *in;
}

/*
 * Parse character literals in the format 'a', '\xaf', '\0', '\077' etc,
 * starting from *in. The position of the character after the last '
 * character is stored in endptr. If no valid conversion can be made,
 * *endptr == in.
 */
static struct token strtochar(char *in, char **endptr)
{
    struct token tok = {NUMBER};
    assert(*in == '\'');

    in++;
    tok.type = basic_type__int;
    tok.d.val.i = escpchar(in, endptr);
    if (**endptr != '\'') {
        error("Invalid character constant %c.", *in);
    }

    *endptr += 1;
    tok.is_char_literal = 1;
    return tok;
}

/*
 * Parse string literal inputs delimited by quotation marks, handling
 * escaped quotes. The input buffer is destructively overwritten while
 * resolving escape sequences.
 */
static struct token strtostr(char *in, char **endptr)
{
    struct token string = {STRING};
    char *start, *str;
    int len = 0;

    start = str = in;
    *endptr = in;

    if (*in++ == '"') {
        while (*in != '"' && *in) {
            *str++ = escpchar(in, &in);
            len++;
        }

        if (*in++ == '"') {
            *str = '\0';
            *endptr = in;
        }
    }

    if (*endptr == start) {
        error("Invalid string literal.");
        exit(1);
    }

    string.d.string = str_register(start, len);
    return string;
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

#define S1(a) (M1(a) && E(1))
#define S2(a, b) (M2(a, b) && E(2))
#define S3(a, b, c) (M3(a, b, c) && E(3))
#define S4(a, b, c, d) (M4(a, b, c, d) && E(4))
#define S5(a, b, c, d, e) (M5(a, b, c, d, e) && E(5))
#define S6(a, b, c, d, e, f) (M6(a, b, c, d, e, f) && E(6))
#define S7(a, b, c, d, e, f, g) (M7(a, b, c, d, e, f, g) && E(7))

#define MATCH(id) \
    do { \
        *endptr = start + basic_token[id].d.string.len; \
        return basic_token[id]; \
    } while (0)

/*
 * Parse string as keyword or identifier. First character should be
 * alphabetic or underscore.
 */
static struct token strtoident(char *in, char **endptr)
{
    char *start = in;
    struct token ident = {IDENTIFIER};

    switch (*in++) {
    case 'a':
        if (S3('u', 't', 'o')) MATCH(AUTO);
        break;
    case 'b':
        if (S4('r', 'e', 'a', 'k')) MATCH(BREAK);
        break;
    case 'c':
        if (S3('a', 's', 'e')) MATCH(CASE);
        if (S3('h', 'a', 'r')) MATCH(CHAR);
        if (M2('o', 'n')) {
            in += 2;
            if (S2('s', 't')) MATCH(CONST);
            if (S5('t', 'i', 'n', 'u', 'e')) MATCH(CONTINUE);
        }
        break;
    case 'd':
        if (S6('e', 'f', 'a', 'u', 'l', 't')) MATCH(DEFAULT);
        if (*in++ == 'o') {
            if (S4('u', 'b', 'l', 'e')) MATCH(DOUBLE);
            if (E(0)) MATCH(DO);
        }
        break;
    case 'e':
        if (S3('l', 's', 'e')) MATCH(ELSE);
        if (S3('n', 'u', 'm')) MATCH(ENUM);
        if (S5('x', 't', 'e', 'r', 'n')) MATCH(EXTERN);
        break;
    case 'f':
        if (S4('l', 'o', 'a', 't')) MATCH(FLOAT);
        if (S2('o', 'r')) MATCH(FOR);
        break;
    case 'g':
        if (S3('o', 't', 'o')) MATCH(GOTO);
        break;
    case 'i':
        if (S1('f')) MATCH(IF);
        if (*in++ == 'n') {
            if (S1('t')) MATCH(INT);
            if (S4('l', 'i', 'n', 'e')) MATCH(INLINE);
        }
        break;
    case 'l':
        if (S3('o', 'n', 'g')) MATCH(LONG);
        break;
    case 'r':
        if (*in++ == 'e') {
            if (S6('g', 'i', 's', 't', 'e', 'r')) MATCH(REGISTER);
            if (S4('t', 'u', 'r', 'n')) MATCH(RETURN);
        }
        break;
    case 's':
        if (S4('h', 'o', 'r', 't')) MATCH(SHORT);
        if (S5('w', 'i', 't', 'c', 'h')) MATCH(SWITCH);
        switch (*in++) {
        case 'i':
            if (S4('g', 'n', 'e', 'd')) MATCH(SIGNED);
            if (S4('z', 'e', 'o', 'f')) MATCH(SIZEOF);
            break;
        case 't':
            if (S4('a', 't', 'i', 'c')) MATCH(STATIC);
            if (S4('r', 'u', 'c', 't')) MATCH(STRUCT);
            break;
        default: break;
        }
        break;
    case 't':
        if (S6('y', 'p', 'e', 'd', 'e', 'f')) MATCH(TYPEDEF);
        break;
    case 'u':
        if (*in++ == 'n') {
            if (S3('i', 'o', 'n')) MATCH(UNION);
            if (S6('s', 'i', 'g', 'n', 'e', 'd')) MATCH(UNSIGNED);
        }
        break;
    case 'v':
        if (*in++ == 'o') {
            if (S2('i', 'd')) MATCH(VOID);
            if (S6('l', 'a', 't', 'i', 'l', 'e')) MATCH(VOLATILE);
        }
        break;
    case 'w':
        if (S4('h', 'i', 'l', 'e')) MATCH(WHILE);
    default:
        break;
    }

    in--;
    while (isident(*in)) {
        in++;
    }

    ident.d.string = str_register(start, in - start);
    ident.is_expandable = 1;
    *endptr = in;
    return ident;
}

static struct token strtoop(char *in, char **endptr)
{
    char *start = in;

    switch (*in++) {
    case '*':
        if (*in == '=') MATCH(MUL_ASSIGN);
        break;
    case '/':
        if (*in == '=') MATCH(DIV_ASSIGN);
        break;
    case '%':
        if (*in == '=') MATCH(MOD_ASSIGN);
        break;
    case '+':
        if (*in == '+') MATCH(INCREMENT);
        if (*in == '=') MATCH(PLUS_ASSIGN);
        break;
    case '-':
        if (*in == '>') MATCH(ARROW);
        if (*in == '-') MATCH(DECREMENT);
        if (*in == '=') MATCH(MINUS_ASSIGN);
        break;
    case '<':
        if (*in == '=') MATCH(LEQ);
        if (*in++ == '<') {
            if (*in == '=') MATCH(LSHIFT_ASSIGN);
            MATCH(LSHIFT);
        }
        break;
    case '>':
        if (*in == '=') MATCH(GEQ);
        if (*in++ == '>') {
            if (*in == '=') MATCH(RSHIFT_ASSIGN);
            MATCH(RSHIFT);
        }
        break;
    case '&':
        if (*in == '=') MATCH(AND_ASSIGN);
        if (*in == '&') MATCH(LOGICAL_AND);
        break;
    case '^':
        if (*in == '=') MATCH(XOR_ASSIGN);
        break;
    case '|':
        if (*in == '=') MATCH(OR_ASSIGN);
        if (*in == '|') MATCH(LOGICAL_OR);
        break;
    case '.':
        if (*in++ == '.' && *in == '.') MATCH(DOTS);
        break;
    case '=':
        if (*in == '=') MATCH(EQ);
        break;
    case '!':
        if (*in == '=') MATCH(NEQ);
        break;
    case '#':
        if (*in == '#') MATCH(TOKEN_PASTE);
        break;
    default:
        break;
    }

    *endptr = start + 1;
    return basic_token[(int) *start];
}

static int skip_spaces(char *in, char **endptr)
{
    char *start = in;

    while (isspace(*in))
        in++;

    *endptr = in;
    return in - start;
}

static size_t write_escaped_char(int c, char *buf)
{
    int i = 0;

    switch (c) {
    case '\a':
        buf[i++] = '\\';
        buf[i++] = 'a';
        break;
    case '\b':
        buf[i++] = '\\';
        buf[i++] = 'b';
        break;
    case '\t':
        buf[i++] = '\\';
        buf[i++] = 't';
        break;
    case '\n':
        buf[i++] = '\\';
        buf[i++] = 'n';
        break;
    case '\v':
        buf[i++] = '\\';
        buf[i++] = 'v';
        break;
    case '\f':
        buf[i++] = '\\';
        buf[i++] = 'f';
        break;
    case '\r':
        buf[i++] = '\\';
        buf[i++] = 'r';
        break;
    case '\0':
        buf[i++] = '\\';
        buf[i++] = '0';
        break;
    case '\\':
    case '\"':
        buf[i++] = '\\';
    default:
        buf[i++] = c;
        break;
    }

    return i;
}

static size_t write_escaped_string(String str, char *buf)
{
    const char *raw;
    size_t i = 0, j = 0;

    raw = str_raw(str);
    buf[j++] = '\"';
    for (i = 0; i < str.len; ++i) {
        j += write_escaped_char(raw[i], buf + j);
    }

    buf[j++] = '\"';
    return j;
}

String tokstr(struct token tok)
{
    static char buf[512];
    char *str;
    size_t len;

    assert(tok.token != PARAM);
    assert(tok.token != EMPTY_ARG);

    switch (tok.token) {
    case NUMBER:
        len = 0;
        if (tok.is_char_literal) {
            assert(is_signed(tok.type));
            buf[len++] = '\'';
            len += write_escaped_char(tok.d.val.i, buf + len);
            buf[len++] = '\'';
        } else if (is_unsigned(tok.type)) {
            len = sprintf(buf,
                (size_of(tok.type) == 8) ? "%luul" : "%luu", tok.d.val.u);
        } else if (is_signed(tok.type)) {
            len = sprintf(buf,
                (size_of(tok.type) == 8) ? "%ldl" : "%ld", tok.d.val.i);
        } else if (is_float(tok.type)) {
            len = sprintf(buf, "%ff", tok.d.val.f);
        } else if (is_double(tok.type)) {
            len = sprintf(buf, "%f", tok.d.val.d);
        } else {
            assert(is_long_double(tok.type));
            len = sprintf(buf, "%Lf", tok.d.val.ld);
        }
        tok.d.string = str_register(buf, len);
        break;
    case STRING:
        str = malloc((tok.d.string.len * 2 + 2) * sizeof(*str));
        len = write_escaped_string(tok.d.string, str);
        tok.d.string = str_register(str, len);
        free(str);
    default: break;
    }

    return tok.d.string;
}

struct token tokenize(char *in, char **endptr)
{
    int ws;
    struct token tok;

    assert(in);
    assert(endptr);

    ws = skip_spaces(in, endptr);
    in = *endptr;

    if (isalpha(*in) || *in == '_') {
        tok = strtoident(in, endptr);
        if (tok.token == INLINE && context.standard == STD_C89) {
            tok.token = IDENTIFIER;
        }
    } else if (*in == '\0') {
        tok = basic_token[END];
    } else if (isdigit(*in) || (*in == '.' && isdigit(in[1]))) {
        tok = strtonum(in, endptr);
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
