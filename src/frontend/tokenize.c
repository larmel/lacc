#if _XOPEN_SOURCE < 700
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup, isblank */
#endif
#include "tokenize.h"
#include "../core/cli.h"
#include "../core/string.h"

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Parse identifier. The C standard specifies a fixed lower limit on the
 * length, which makes it possible to store identifiers in a static buffer.
 */
/*static const char *strtoident(char *in, char **endptr)
{
    static char identifier[64];
    char c, *str;

    *endptr = in;

    str = identifier;
    c = *in++;

    if (isalpha(c) || c == '_') {
        do {
            *str++ = c;
            c = *in++;
        } while (isalpha(c) || isdigit(c) || c == '_');
        if (isspace(c) || !isalnum(c)) {
            *str = '\0';
            *endptr = in - 1;
        }
    }

    return identifier;
}*/

/* Parse integer literal in the format '1234', '0x123', '077' using strtol,
 * then skip any type suffix (uUlL). The type is discarded.
 */
static long strtonum(char *in, char **endptr)
{
    long value;
    char *end;

    value = strtol(in, &end, 0);

    if (end != in) {
        if (*end == 'u' || *end == 'U') end++;
        if (*end == 'l' || *end == 'L') end++;
        if (*end == 'l' || *end == 'L') end++;
    }

    if (endptr)
        *endptr = end;

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
    static char value;

    *endptr = in;
    if (*in++ == '\'') {
        value = escpchar(in, &in);
        if (*in++ == '\'') {
            *endptr = in;
        }
    }

    return value;
}

/* Parse string literal inputs delimited by quotation marks, handling escaped
 * quotes. The input buffer is destructively overwritten while resolving escape
 * sequences. Concatenate string literals separated by whitespace.
 */
static const char *strtostr(char *in, char **endptr)
{
    char *start, *str;

    start = str = in;
    *endptr = in;

    do {
        if (*in++ == '"') {
            while (*in != '"' && *in) {
                *str++ = escpchar(in, &in);
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

    return start;
}

/* Parse string as whitespace tokens, consuming space and tab characters.
 */
static void strtospace(char *in, char **endptr)
{
    while (isblank(*in))
        in++;

    *endptr = in;
}

/* Valid identifier character, except in the first position which does not
 * allow numbers.
 */
#define isident(c) (isalnum(c) || (c) == '_')

/* Macros to make state state machine implementation of identifier and operator
 * tokenization simpler.
 */
#define at(c) (*in == (c) && in++)
#define get(c) (*in++ == (c))
#define end() !isident(*in)
#define ret(t, s) \
    do { \
        *endptr = in; \
        res.token = t; \
        res.strval = s; \
        return res; \
    } while (0)

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

/* Parse string as keyword or identifier. First character should be alphabetic
 * or underscore.
 */
static struct token strtoident(char *in, char **endptr)
{
    struct token res = {IDENTIFIER};

    switch (*in++) {
    case 'a':
        if (S3('u', 't', 'o'))                      ret(AUTO, "auto");
        break;
    case 'b':
        if (S4('r', 'e', 'a', 'k'))                 ret(BREAK, "break");
        break;
    case 'c':
        if (S3('a', 's', 'e'))                      ret(CASE, "case");
        if (S3('h', 'a', 'r'))                      ret(CHAR, "char");
        if (at('o') && get('n')) {
            if (S2('s', 't'))                       ret(CONST, "const");
            if (S5('t', 'i', 'n', 'u', 'e'))        ret(CONTINUE, "continue");
        }
        break;
    case 'd':
        if (S6('e', 'f', 'a', 'u', 'l', 't'))       ret(DEFAULT, "default");
        if (at('o')) {
            if (S4('u', 'b', 'l', 'e'))             ret(DOUBLE, "double");
            if (end())                              ret(DO, "do");
        }
        break;
    case 'e':
        if (S3('l', 's', 'e'))                      ret(ELSE, "else");
        if (S3('n', 'u', 'm'))                      ret(ENUM, "enum");
        if (S5('x', 't', 'e', 'r', 'n'))            ret(EXTERN, "extern");
        break;
    case 'f':
        if (S4('l', 'o', 'a', 't'))                 ret(FLOAT, "float");
        if (S2('o', 'r'))                           ret(FOR, "for");
        break;
    case 'g':
        if (S3('o', 't', 'o'))                      ret(GOTO, "goto");
        break;
    case 'i':
        if (S1('f'))                                ret(IF, "if");
        if (S2('n', 't'))                           ret(INT, "int");
        break;
    case 'l':
        if (S3('o', 'n', 'g'))                      ret(LONG, "long");
        break;
    case 'r':
        if (at('e')) {
            if (S6('g', 'i', 's', 't', 'e', 'r'))   ret(REGISTER, "register");
            if (S4('t', 'u', 'r', 'n'))             ret(RETURN, "return");
        }
        break;
    case 's':
        if (S4('h', 'o', 'r', 't'))                 ret(SHORT, "short");
        if (S5('w', 'i', 't', 'c', 'h'))            ret(SWITCH, "switch");
        if (at('i')) {
            if (S4('g', 'n', 'e', 'd'))             ret(SIGNED, "signed");
            if (S4('z', 'e', 'o', 'f'))             ret(SIZEOF, "sizeof");
        }
        if (at('t')) {
            if (S4('a', 't', 'i', 'c'))             ret(STATIC, "static");
            if (S4('r', 'u', 'c', 't'))             ret(STRUCT, "struct");
        }
        break;
    case 't':
        if (S6('y', 'p', 'e', 'd', 'e', 'f'))       ret(TYPEDEF, "typedef");
        break;
    case 'u':
        if (at('n')) {
            if (S3('i', 'o', 'n'))                  ret(UNION, "union");
            if (S6('s', 'i', 'g', 'n', 'e', 'd'))   ret(UNSIGNED, "unsigned");
        }
        break;
    case 'v':
        if (at('o')) {
            if (S2('i', 'd'))                       ret(VOID, "void");
            if (S6('l', 'a', 't', 'i', 'l', 'e'))   ret(VOLATILE, "volatile");
        }
        break;
    case 'w':
        if (S4('h', 'i', 'l', 'e'))                 ret(WHILE, "while");
    default:
        break;
    }

    /* Fallthrough means we have consumed at least one character, and the token
     * should be identifier. Backtrack one position to correct a get() that
     * moved us past the end. */
    in--;

    while (isident(*in))
        in++;

    *endptr = in;
    return res;
}

static struct token strtoop(char *in, char **endptr)
{
    struct token res = {0};

    switch (*in++) {
    case '*':
        if (at('=')) ret(MUL_ASSIGN, "*=");
        ret('*', "*");
    case '/':
        if (at('=')) ret(DIV_ASSIGN, "/=");
        ret('/', "/");
    case '%':
        if (at('=')) ret(MOD_ASSIGN, "\x25=");
        ret('%', "\x25");
    case '+':
        if (at('+')) ret(INCREMENT, "++");
        if (at('=')) ret(PLUS_ASSIGN, "+=");
        ret('+', "+");
    case '-':
        if (at('>')) ret(ARROW, "->");
        if (at('-')) ret(DECREMENT, "--");
        if (at('=')) ret(MINUS_ASSIGN, "-=");
        ret('-', "-");
    case '<':
        if (at('=')) ret(LEQ, "<=");
        if (at('<')) {
            if (at('=')) ret(LSHIFT_ASSIGN, "<<=");
            ret(LSHIFT, "<<");
        }
        ret('<', "<");
    case '>':
        if (at('=')) ret(GEQ, ">=");
        if (at('>')) {
            if (at('=')) ret(RSHIFT_ASSIGN, ">>=");
            ret(RSHIFT, ">>");
        }
        ret('>', ">");
    case '&':
        if (at('=')) ret(AND_ASSIGN, "&=");
        if (at('&')) ret(LOGICAL_AND, "||");
        ret('&', "&");
    case '^':
        if (at('=')) ret(XOR_ASSIGN, "^=");
        ret('^', "^");
    case '|':
        if (at('=')) ret(OR_ASSIGN, "|=");
        if (at('|')) ret(LOGICAL_OR, "||");
        ret('|', "|");
    case '.':
        if (at('.') && get('.')) ret(DOTS, "...");
        ret('.', ".");
    case '=':
        if (at('=')) ret(EQ, "==");
        ret('=', "=");
    case '!':
        if (at('=')) ret(NEQ, "!=");
        ret('!', "!");
    case '#':
        if (at('#')) ret(TOKEN_PASTE, "##");
        ret('#', "#");
    case '(': ret('(', "(");
    case ')': ret(')', ")");
    case ';': ret(';', ";");
    case ':': ret(':', ":");
    case '{': ret('{', "{");
    case '}': ret('}', "}");
    case '[': ret('[', "[");
    case ']': ret(']', "]");
    case ',': ret(',', ",");
    case '?': ret('?', "?");
    case '~': ret('~', "~");
    default:
        break;
    }

    return res;
}

struct token
    token_end = { END, "$" },
    token_newline = { NEWLINE, "\n" };

struct token tokenize(char *in, char **endptr)
{
    struct token res = {0};

    assert(endptr);
    assert(in);

    *endptr = in;

    if (*in == '\0') {
        return token_end;
    } else if (isspace(*in)) {
        res.token = SPACE;
        strtospace(in, endptr);
        if (*endptr != in) {
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
    } else if (isalpha(*in) || *in == '_') {
        res = strtoident(in, endptr);
        assert(*endptr != in);
        if (res.token == IDENTIFIER) {
            res.strval = str_register_n(in, *endptr - in);
        }
        return res;
    } else if (isdigit(*in)) {
        res.intval = strtonum(in, endptr);
        if (*endptr != in) {
            res.token = INTEGER_CONSTANT;
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
    } else if (*in == '"') {
        res.strval = strtostr(in, endptr);
        if (*endptr != in) {
            res.token = STRING;
            res.strval = str_register(res.strval);
            return res;
        }
    } else if (*in == '\'') {
        res.intval = strtochar(in, endptr);
        if (*endptr != in) {
            res.token = INTEGER_CONSTANT;
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
    }

    return strtoop(in, endptr);
}
