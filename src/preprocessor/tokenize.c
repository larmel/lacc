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

const struct token
    token_end = {END, {"$", 1}},
    token_newline = {NEWLINE, {"\n", 1}};

const struct string reserved[] = {
/* 0x00 */  {"$", 1},        {"auto", 4},     {"break", 5},    {"case", 4},
            {"char", 4},     {"const", 5},    {"continue", 8}, {"default", 7},
/* 0x08 */  {"do", 2},       {"double", 6},   {"\n", 1},       {"else", 4},
            {"enum", 4},     {"extern", 6},   {"float", 5},    {"for", 3}, 
/* 0x10 */  {"goto", 4},     {"if", 2},       {"int", 3},      {"long", 4},
            {"register", 8}, {"return", 6},   {"short", 5},    {"signed", 6},
/* 0x18 */  {"sizeof", 6},   {"static", 6},   {"struct", 6},   {"switch", 6},
            {"typedef", 7},  {"union", 5},    {"unsigned", 8}, {"void", 4},
/* 0x20 */  {" ", 1},        {"!", 1},        {"volatile", 8}, {"#", 1},
            {"while", 5},    {"\x25", 1},     {"&", 1},        {0},
/* 0x28 */  {"(", 1},        {")", 1},        {"*", 1},        {"+", 1},
            {",", 1},        {"-", 1},        {".", 1},        {"/", 1},
/* 0x30 */  {0},             {0},             {0},             {0},
            {0},             {0},             {0},             {0},
/* 0x38 */  {0},             {0},             {":", 1},        {";", 1},
            {"<", 1},        {"=", 1},        {">", 1},        {"?", 1},
/* 0x40 */  {"...", 3},      {"||", 2},       {"&&", 2},       {"<=", 2},
            {">=", 2},       {"==", 2},       {"!=", 2},       {"->", 2},
/* 0x48 */  {"++", 2},       {"--", 2},       {"<<", 2},       {">>", 2},
            {"*=", 2},       {"/=", 2},       {"\x25=", 2},    {"+=", 2},
/* 0x50 */  {"-=", 2},       {"<<=", 3},      {">>=", 3},      {"&=", 2},
            {"^=", 2},       {"|=", 2},       {"##", 2},       {0},
/* 0x58 */  {0},             {0},             {0},             {"[", 1},
            {0},             {"]", 1},        {"^", 1},        {0},
/* 0x60 */  {0},             {0},             {0},             {0},
            {0},             {0},             {0},             {0},
/* 0x68 */  {0},             {0},             {0},             {0},
            {0},             {0},             {0},             {0},
/* 0x70 */  {0},             {0},             {0},             {0},
            {0},             {0},             {0},             {0},
/* 0x78 */  {0},             {0},             {0},             {"{", 1},
            {"|", 1},        {"}", 1},        {"~", 1},        {0},
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
static long strtonum(char *in, char **endptr)
{
    long value;
    char *e;

    value = strtol(in, &e, 0);

    if (e != in) {
        if (*e == 'u' || *e == 'U') e++;
        if (*e == 'l' || *e == 'L') e++;
        if (*e == 'l' || *e == 'L') e++;
    }

    if (endptr)
        *endptr = e;

    return value;
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
static char strtochar(char *in, char **endptr)
{
    char value;

    assert(*in == '\'');

    in++;
    value = escpchar(in, endptr);
    if (**endptr != '\'') {
        error("Invalid character constant %c.", *in);
    }

    *endptr += 1;
    return value;
}

/* Parse string literal inputs delimited by quotation marks, handling escaped
 * quotes. The input buffer is destructively overwritten while resolving escape
 * sequences. Concatenate string literals separated by whitespace.
 */
static struct string strtostr(char *in, char **endptr)
{
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
        while (isspace(*in)) in++;
        if (*in != '"')
            break;
    } while (1);

    if (*endptr == start) {
        error("Invalid string literal.");
        exit(1);
    }

    return str_register(start, len);
}

/* Parse string as whitespace tokens, consuming space and tab characters.
 */
static void strtospace(char *in, char **endptr)
{
    do in++;
    while (isspace(*in));
    *endptr = in;
}

/* Parse string as keyword or identifier. First character should be alphabetic
 * or underscore.
 */
static enum token_type strtoident(char *in, char **endptr)
{
    *endptr = in;
    switch (*(*endptr)++) {
    case 'a':
        if (S3('u', 't', 'o')) return AUTO;
        break;
    case 'b':
        if (S4('r', 'e', 'a', 'k')) return BREAK;
        break;
    case 'c':
        if (S3('a', 's', 'e')) return CASE;
        if (S3('h', 'a', 'r')) return CHAR;
        if (at('o') && get('n')) {
            if (S2('s', 't')) return CONST;
            if (S5('t', 'i', 'n', 'u', 'e')) return CONTINUE;
        }
        break;
    case 'd':
        if (S6('e', 'f', 'a', 'u', 'l', 't')) return DEFAULT;
        if (at('o')) {
            if (S4('u', 'b', 'l', 'e')) return DOUBLE;
            if (end()) return DO;
        }
        break;
    case 'e':
        if (S3('l', 's', 'e')) return ELSE;
        if (S3('n', 'u', 'm')) return ENUM;
        if (S5('x', 't', 'e', 'r', 'n')) return EXTERN;
        break;
    case 'f':
        if (S4('l', 'o', 'a', 't')) return FLOAT;
        if (S2('o', 'r')) return FOR;
        break;
    case 'g':
        if (S3('o', 't', 'o')) return GOTO;
        break;
    case 'i':
        if (S1('f')) return IF;
        if (S2('n', 't')) return INT;
        break;
    case 'l':
        if (S3('o', 'n', 'g')) return LONG;
        break;
    case 'r':
        if (at('e')) {
            if (S6('g', 'i', 's', 't', 'e', 'r')) return REGISTER;
            if (S4('t', 'u', 'r', 'n')) return RETURN;
        }
        break;
    case 's':
        if (S4('h', 'o', 'r', 't')) return SHORT;
        if (S5('w', 'i', 't', 'c', 'h')) return SWITCH;
        if (at('i')) {
            if (S4('g', 'n', 'e', 'd')) return SIGNED;
            if (S4('z', 'e', 'o', 'f')) return SIZEOF;
        }
        if (at('t')) {
            if (S4('a', 't', 'i', 'c')) return STATIC;
            if (S4('r', 'u', 'c', 't')) return STRUCT;
        }
        break;
    case 't':
        if (S6('y', 'p', 'e', 'd', 'e', 'f')) return TYPEDEF;
        break;
    case 'u':
        if (at('n')) {
            if (S3('i', 'o', 'n')) return UNION;
            if (S6('s', 'i', 'g', 'n', 'e', 'd')) return UNSIGNED;
        }
        break;
    case 'v':
        if (at('o')) {
            if (S2('i', 'd')) return VOID;
            if (S6('l', 'a', 't', 'i', 'l', 'e')) return VOLATILE;
        }
        break;
    case 'w':
        if (S4('h', 'i', 'l', 'e')) return WHILE;
    default:
        break;
    }

    /* Fallthrough means we have consumed at least one character, and the token
     * should be identifier. Backtrack one position to correct a get() that
     * moved us past the end. */
    (*endptr)--;

    while (isident(**endptr))
        (*endptr)++;

    return IDENTIFIER;
}

static enum token_type strtoop(char *in, char **endptr)
{
    *endptr = in;
    switch (*(*endptr)++) {
    case '*':
        if (at('=')) return MUL_ASSIGN;
        break;
    case '/':
        if (at('=')) return DIV_ASSIGN;
        break;
    case '%':
        if (at('=')) return MOD_ASSIGN;
        break;
    case '+':
        if (at('+')) return INCREMENT;
        if (at('=')) return PLUS_ASSIGN;
        break;
    case '-':
        if (at('>')) return ARROW;
        if (at('-')) return DECREMENT;
        if (at('=')) return MINUS_ASSIGN;
        break;
    case '<':
        if (at('=')) return LEQ;
        if (at('<')) {
            if (at('=')) return LSHIFT_ASSIGN;
            return LSHIFT;
        }
        break;
    case '>':
        if (at('=')) return GEQ;
        if (at('>')) {
            if (at('=')) return RSHIFT_ASSIGN;
            return RSHIFT;
        }
        break;
    case '&':
        if (at('=')) return AND_ASSIGN;
        if (at('&')) return LOGICAL_AND;
        break;
    case '^':
        if (at('=')) return XOR_ASSIGN;
        break;
    case '|':
        if (at('=')) return OR_ASSIGN;
        if (at('|')) return LOGICAL_OR;
        break;
    case '.':
        if (at('.') && get('.')) return DOTS;
        break;
    case '=':
        if (at('=')) return EQ;
        break;
    case '!':
        if (at('=')) return NEQ;
        break;
    case '#':
        if (at('#')) return TOKEN_PASTE;
        break;
    default:
        break;
    }

    *endptr = in + 1;
    return *in;
}

struct token tokenize(char *in, char **endptr)
{
    struct token res = {0};
    assert(in && endptr);

    *endptr = in;
    if (*in == '\0') {
        res = token_end;
    } else if (isspace(*in)) {
        res.token = SPACE;
        strtospace(in, endptr);
        assert(*endptr != in);
        res.strval = str_register(in, *endptr - in);
    } else if (isalpha(*in) || *in == '_') {
        res.token = strtoident(in, endptr);
        assert(*endptr != in);
        res.strval =
            (res.token == IDENTIFIER) ?
                str_register(in, *endptr - in) :
                reserved[res.token];
    } else if (isdigit(*in)) {
        res.token = INTEGER_CONSTANT;
        res.intval = strtonum(in, endptr);
        assert(*endptr != in);
        res.strval = str_register(in, *endptr - in);
    } else if (*in == '"') {
        res.token = STRING;
        res.strval = strtostr(in, endptr);
    } else if (*in == '\'') {
        res.token = INTEGER_CONSTANT;
        res.intval = strtochar(in, endptr);
        assert(*endptr != in);
        res.strval = str_register(in, *endptr - in);
    } else {
        res.token = strtoop(in, endptr);
        res.strval = reserved[res.token];
        assert(*endptr != in);
    }

    return res;
}
