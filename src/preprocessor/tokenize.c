#if _XOPEN_SOURCE < 600
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 600 /* isblank */
#endif
#include "strtab.h"
#include "tokenize.h"
#include <lacc/cli.h>

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define S(s) {(s), sizeof(s) - 1}
#define T(t, s) {(t), 0, S(s)}

const struct token basic_token[] = {
/* 0x00 */  T(END, "$"),                T(AUTO, "auto"),
            T(BREAK, "break"),          T(CASE, "case"),
            T(CHAR, "char"),            T(CONST, "const"),
            T(CONTINUE, "continue"),    T(DEFAULT, "default"),
/* 0x08 */  T(DO, "do"),                T(DOUBLE, "double"),
            T(NEWLINE, "\n"),           T(ELSE, "else"),
            T(ENUM, "enum"),            T(EXTERN, "extern"),
            T(FLOAT, "float"),          T(FOR, "for"), 
/* 0x10 */  T(GOTO, "goto"),            T(IF, "if"),
            T(INT, "int"),              T(LONG, "long"),
            T(REGISTER, "register"),    T(RETURN, "return"),
            T(SHORT, "short"),          T(SIGNED, "signed"),
/* 0x18 */  T(SIZEOF, "sizeof"),        T(STATIC, "static"),
            T(STRUCT, "struct"),        T(SWITCH, "switch"),
            T(TYPEDEF, "typedef"),      T(UNION, "union"),
            T(UNSIGNED, "unsigned"),    T(VOID, "void"),
/* 0x20 */  {0},                        T(NOT, "!"),
            T(VOLATILE, "volatile"),    T(HASH, "#"),
            T(WHILE, "while"),          T(MODULO, "%"),
            T(AND, "&"),                {0},
/* 0x28 */  T(OPEN_PAREN, "("),         T(CLOSE_PAREN, ")"),
            T(STAR, "*"),               T(PLUS, "+"),
            T(COMMA, ","),              T(MINUS, "-"),
            T(DOT, "."),                T(SLASH, "/"),
/* 0x30 */  {0},                        {0},
            {0},                        {0},
            {0},                        {0},
            {0},                        {0},
/* 0x38 */  {0},                        {0},
            T(COLON, ":"),              T(SEMICOLON, ";"),
            T(LT, "<"),                 T(ASSIGN, "="),
            T(GT, ">"),                 T(QUESTION, "?"),
/* 0x40 */  T(DOTS, "..."),             T(LOGICAL_OR, "||"),
            T(LOGICAL_AND, "&&"),       T(LEQ, "<="),
            T(GEQ, ">="),               T(EQ, "=="),
            T(NEQ, "!="),               T(ARROW, "->"),
/* 0x48 */  T(INCREMENT, "++"),         T(DECREMENT, "--"),
            T(LSHIFT, "<<"),            T(RSHIFT, ">>"),
            T(MUL_ASSIGN, "*="),        T(DIV_ASSIGN, "/="),
            T(MOD_ASSIGN, "%="),        T(PLUS_ASSIGN, "+="),
/* 0x50 */  T(MINUS_ASSIGN, "-="),      T(LSHIFT_ASSIGN, "<<="),
            T(RSHIFT_ASSIGN, ">>="),    T(AND_ASSIGN, "&="),
            T(XOR_ASSIGN, "^="),        T(OR_ASSIGN, "|="),
            T(TOKEN_PASTE, "##"),       {0},
/* 0x58 */  {0},                        {0},
            {0},                        T(OPEN_BRACKET, "["),
            {0},                        T(CLOSE_BRACKET, "]"),
            T(XOR, "^"),                {0},
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
            {INTEGER_CONSTANT},         {IDENTIFIER},
            {STRING},                   {0},
/* 0x78 */  {0},                        {0},
            {0},                        T(OPEN_CURLY, "{"),
            T(OR, "|"),                 T(CLOSE_CURLY, "}"),
            T(NEG, "~"),                {0},
};

/* Valid identifier character, except in the first position which does not
 * allow numbers.
 */
#define isident(c) (isalnum(c) || (c) == '_')

/* Macros to make state state machine implementation of identifier and operator
 * tokenization simpler.
 */
#define at(c) (**endptr == (c) && (*endptr)++)
#define get(c) (*(*endptr)++ == (c))
#define end() !isident(**endptr)

#define S1(a) (at(a) && end())
#define S2(a, b) (at(a) && get(b) && end())
#define S3(a, b, c) (at(a) && get(b) && get(c) && end())
#define S4(a, b, c, d) (at(a) && get(b) && get(c) && get(d) && end())
#define S5(a, b, c, d, e) \
    (at(a) && get(b) && get(c) && get(d) && get(e) && end())
#define S6(a, b, c, d, e, f) \
    (at(a) && get(b) && get(c) && get(d) && get(e) && get(f) && end())
#define S7(a, b, c, d, e, f, g) \
    (at(a) && get(b) && get(c) && get(d) && get(e) && get(f) && get(g) && end())

/* Parse integer literal in the format '1234', '0x123', '077' using strtol,
 * then skip any type suffix (uUlL). The type is discarded.
 */
static struct token strtonum(char *in, char **endptr)
{
    struct token integer = {INTEGER_CONSTANT};
    char *e;

    integer.intval = strtol(in, &e, 0);
    if (e != in) {
        if (*e == 'u' || *e == 'U') e++;
        if (*e == 'l' || *e == 'L') e++;
        if (*e == 'l' || *e == 'L') e++;
    }

    *endptr = e;
    integer.strval = str_register(in, *endptr - in);
    return integer;
}

/* Parse character escape code, including octal and hexadecimal number
 * literals. Unescaped characters are returned as-is. Invalid escape sequences
 * continues with an error, consuming only the backslash.
 */
static char escpchar(char *in, char **endptr)
{
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
        case '0':
            if (isdigit(in[2]) && in[2] < '8')
                return (char) strtol(&in[1], endptr, 8);
            return '\0';
        case 'x':
            return (char) strtol(&in[2], endptr, 16);
        default:
            error("Invalid escape sequence '\\%c'.", in[1]);
            return in[1];
        }
    }

    *endptr = in + 1;
    return *in;
}

/* Parse character literals in the format 'a', '\xaf', '\0', '\077' etc,
 * starting from *in. The position of the character after the last ' character
 * is stored in endptr. If no valid conversion can be made, *endptr == in.
 */
static struct token strtochar(char *in, char **endptr)
{
    struct token integer = {INTEGER_CONSTANT};
    char value, *start = in;

    assert(*in == '\'');

    in++;
    value = escpchar(in, endptr);
    if (**endptr != '\'') {
        error("Invalid character constant %c.", *in);
    }

    *endptr += 1;
    integer.intval = value;
    integer.strval = str_register(start, *endptr - start);
    return integer;
}

/* Parse string literal inputs delimited by quotation marks, handling escaped
 * quotes. The input buffer is destructively overwritten while resolving escape
 * sequences. Concatenate string literals separated by whitespace.
 */
static struct token strtostr(char *in, char **endptr)
{
    struct token string = {STRING};
    char *start, *str;
    int len = 0;

    start = str = in;
    *endptr = in;

    do {
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

        /* See if there is another string after this one. */
        while (isblank(*in)) in++;
        if (*in != '"')
            break;
    } while (1);

    if (*endptr == start) {
        error("Invalid string literal.");
        exit(1);
    }

    string.strval = str_register(start, len);
    return string;
}

/* Parse string as keyword or identifier. First character should be alphabetic
 * or underscore.
 */
static struct token strtoident(char *in, char **endptr)
{
    struct token ident = {IDENTIFIER};

    *endptr = in;
    switch (*(*endptr)++) {
    case 'a':
        if (S3('u', 't', 'o')) return basic_token[AUTO];
        break;
    case 'b':
        if (S4('r', 'e', 'a', 'k')) return basic_token[BREAK];
        break;
    case 'c':
        if (S3('a', 's', 'e')) return basic_token[CASE];
        if (S3('h', 'a', 'r')) return basic_token[CHAR];
        if (at('o') && get('n')) {
            if (S2('s', 't')) return basic_token[CONST];
            if (S5('t', 'i', 'n', 'u', 'e')) return basic_token[CONTINUE];
        }
        break;
    case 'd':
        if (S6('e', 'f', 'a', 'u', 'l', 't')) return basic_token[DEFAULT];
        if (at('o')) {
            if (S4('u', 'b', 'l', 'e')) return basic_token[DOUBLE];
            if (end()) return basic_token[DO];
        }
        break;
    case 'e':
        if (S3('l', 's', 'e')) return basic_token[ELSE];
        if (S3('n', 'u', 'm')) return basic_token[ENUM];
        if (S5('x', 't', 'e', 'r', 'n')) return basic_token[EXTERN];
        break;
    case 'f':
        if (S4('l', 'o', 'a', 't')) return basic_token[FLOAT];
        if (S2('o', 'r')) return basic_token[FOR];
        break;
    case 'g':
        if (S3('o', 't', 'o')) return basic_token[GOTO];
        break;
    case 'i':
        if (S1('f')) return basic_token[IF];
        if (S2('n', 't')) return basic_token[INT];
        break;
    case 'l':
        if (S3('o', 'n', 'g')) return basic_token[LONG];
        break;
    case 'r':
        if (at('e')) {
            if (S6('g', 'i', 's', 't', 'e', 'r')) return basic_token[REGISTER];
            if (S4('t', 'u', 'r', 'n')) return basic_token[RETURN];
        }
        break;
    case 's':
        if (S4('h', 'o', 'r', 't')) return basic_token[SHORT];
        if (S5('w', 'i', 't', 'c', 'h')) return basic_token[SWITCH];
        if (at('i')) {
            if (S4('g', 'n', 'e', 'd')) return basic_token[SIGNED];
            if (S4('z', 'e', 'o', 'f')) return basic_token[SIZEOF];
        }
        if (at('t')) {
            if (S4('a', 't', 'i', 'c')) return basic_token[STATIC];
            if (S4('r', 'u', 'c', 't')) return basic_token[STRUCT];
        }
        break;
    case 't':
        if (S6('y', 'p', 'e', 'd', 'e', 'f')) return basic_token[TYPEDEF];
        break;
    case 'u':
        if (at('n')) {
            if (S3('i', 'o', 'n')) return basic_token[UNION];
            if (S6('s', 'i', 'g', 'n', 'e', 'd')) return basic_token[UNSIGNED];
        }
        break;
    case 'v':
        if (at('o')) {
            if (S2('i', 'd')) return basic_token[VOID];
            if (S6('l', 'a', 't', 'i', 'l', 'e')) return basic_token[VOLATILE];
        }
        break;
    case 'w':
        if (S4('h', 'i', 'l', 'e')) return basic_token[WHILE];
    default:
        break;
    }

    /* Fallthrough means we have consumed at least one character, and the token
     * should be identifier. Backtrack one position to correct a get() that
     * moved us past the end. */
    (*endptr)--;

    while (isident(**endptr))
        (*endptr)++;

    ident.strval = str_register(in, *endptr - in);
    return ident;
}

static struct token strtoop(char *in, char **endptr)
{
    *endptr = in;
    switch (*(*endptr)++) {
    case '*':
        if (at('=')) return basic_token[MUL_ASSIGN];
        break;
    case '/':
        if (at('=')) return basic_token[DIV_ASSIGN];
        break;
    case '%':
        if (at('=')) return basic_token[MOD_ASSIGN];
        break;
    case '+':
        if (at('+')) return basic_token[INCREMENT];
        if (at('=')) return basic_token[PLUS_ASSIGN];
        break;
    case '-':
        if (at('>')) return basic_token[ARROW];
        if (at('-')) return basic_token[DECREMENT];
        if (at('=')) return basic_token[MINUS_ASSIGN];
        break;
    case '<':
        if (at('=')) return basic_token[LEQ];
        if (at('<')) {
            if (at('=')) return basic_token[LSHIFT_ASSIGN];
            return basic_token[LSHIFT];
        }
        break;
    case '>':
        if (at('=')) return basic_token[GEQ];
        if (at('>')) {
            if (at('=')) return basic_token[RSHIFT_ASSIGN];
            return basic_token[RSHIFT];
        }
        break;
    case '&':
        if (at('=')) return basic_token[AND_ASSIGN];
        if (at('&')) return basic_token[LOGICAL_AND];
        break;
    case '^':
        if (at('=')) return basic_token[XOR_ASSIGN];
        break;
    case '|':
        if (at('=')) return basic_token[OR_ASSIGN];
        if (at('|')) return basic_token[LOGICAL_OR];
        break;
    case '.':
        if (at('.') && get('.')) return basic_token[DOTS];
        break;
    case '=':
        if (at('=')) return basic_token[EQ];
        break;
    case '!':
        if (at('=')) return basic_token[NEQ];
        break;
    case '#':
        if (at('#')) return basic_token[TOKEN_PASTE];
        break;
    default:
        break;
    }

    *endptr = in + 1;
    return basic_token[(int) *in];
}

/* Parse string as whitespace tokens, consuming space and tabs. Return
 * number of characters.
 */
static int skip_spaces(char *in, char **endptr)
{
    char *start = in;

    while (isblank(*in))
        in++;

    *endptr = in;
    return in - start;
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
    } else if (*in == '\0') {
        tok = basic_token[END];
    } else if (isdigit(*in)) {
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
