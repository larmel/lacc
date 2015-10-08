#if _XOPEN_SOURCE < 700
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup, isblank */
#endif
#include "core/error.h"
#include "core/string.h"
#include "preprocess.h"

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Parse identifier. The C standard specifies a fixed lower limit on the
 * length, which makes it possible to store identifiers in a static buffer.
 */
static const char *strtoident(char *in, char **endptr)
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
}

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

static struct token keywords[] = {
    { AUTO, "auto" },
    { BREAK, "break" },
    { CASE, "case" },
    { CHAR, "char" },
    { CONST, "const" },
    { CONTINUE, "continue" },
    { DEFAULT, "default" },
    { DOUBLE, "double" },
    { DO, "do" },
    { ELSE, "else" },
    { ENUM, "enum" },
    { EXTERN, "extern" },
    { FLOAT, "float" },
    { FOR, "for" },
    { GOTO, "goto" },
    { IF, "if" },
    { INT, "int" },
    { LONG, "long" },
    { REGISTER, "register" },
    { RETURN, "return" },
    { SHORT, "short" },
    { SIGNED, "signed" },
    { SIZEOF, "sizeof" },
    { STATIC, "static" },
    { STRUCT, "struct" },
    { SWITCH, "switch" },
    { TYPEDEF, "typedef" },
    { UNION, "union" },
    { UNSIGNED, "unsigned" },
    { VOID, "void" },
    { VOLATILE, "volatile" },
    { WHILE, "while" }
};

static struct token operators[] = {
    { MUL_ASSIGN, "*=", },
    { DIV_ASSIGN, "/=", },
    { MOD_ASSIGN, "\x25=" },
    { PLUS_ASSIGN, "+=" },
    { MINUS_ASSIGN, "-=" },
    { LSHIFT_ASSIGN, "<<=" },
    { RSHIFT_ASSIGN, ">>=" },
    { AND_ASSIGN, "&=" },
    { XOR_ASSIGN, "^=" },
    { OR_ASSIGN, "|=" },
    { DOTS, "..." },
    { LOGICAL_OR, "||" },
    { LOGICAL_AND, "&&" },
    { LEQ, "<=" },
    { GEQ, ">=" },
    { EQ, "==" },
    { NEQ, "!=" },
    { ARROW, "->" },
    { INCREMENT, "++" },
    { DECREMENT, "--" },
    { LSHIFT, "<<" },
    { RSHIFT, ">>" },
    { TOKEN_PASTE, "##" },
    { '|', "|" },
    { '&', "&" },
    { '^', "^" },
    { '%', "\x25" },
    { '<', "<" },
    { '>', ">" },
    { '(', "(" },
    { ')', ")" },
    { ';', ";" },
    { ':', ":" },
    { '{', "{" },
    { '}', "}" },
    { '[', "[" },
    { ']', "]" },
    { ',', "," },
    { '.', "." },
    { '=', "=" },
    { '*', "*" },
    { '/', "/" },
    { '+', "+" },
    { '-', "-" },
    { '!', "!" },
    { '?', "?" },
    { '~', "~" },
    { '#', "#" }
};

static struct token
    end = { END, "$" },
    newline = { NEWLINE, "\n" };

struct token tokenize(char *in, char **endptr)
{
    struct token res = {0};

    assert(endptr);
    assert(in);

    if (*in == '\0') {
        *endptr = in;
        return end;
    } else if (isspace(*in)) {
        res.token = SPACE;
        strtospace(in, endptr);
        if (*endptr != in) {
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
        error("Invalid whitespace sequence: '%s'.", in);
    } else if (isalpha(*in) || *in == '_') {
        int n = 0;
        for (; n < sizeof(keywords) / sizeof(keywords[0]); ++n) {
            int length = strlen(keywords[n].strval);
            if (!strncmp(in, keywords[n].strval, length)) {
                if (isalnum(in[length]) || in[length] == '_') {
                    /* Reserved word is a prefix of an identifier. */
                    break;
                }
                *endptr = in + length;
                return keywords[n];
            }
        }
        res.strval = strtoident(in, endptr);
        if (*endptr != in) {
            res.token = IDENTIFIER;
            res.strval = str_register(res.strval);
            return res;
        }
        error("Invalid identifier: '%s'.", in);
    } else if (isdigit(*in)) {
        res.intval = strtonum(in, endptr);
        if (*endptr != in) {
            res.token = INTEGER_CONSTANT;
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
        error("Invalid number literal: '%s'.", in);
    } else if (*in == '"') {
        res.strval = strtostr(in, endptr);
        if (*endptr != in) {
            res.token = STRING;
            res.strval = str_register(res.strval);
            return res;
        }
        error("Invalid string literal: '%s'.", in);
    } else if (*in == '\'') {
        res.intval = strtochar(in, endptr);
        if (*endptr != in) {
            res.token = INTEGER_CONSTANT;
            res.strval = str_register_n(in, *endptr - in);
            return res;
        }
        error("Invalid character literal: '%s'.", in);
    } else {
        int i = 0;
        for (; i < sizeof(operators) / sizeof(operators[0]); ++i) {
            int len = strlen(operators[i].strval);
            if (!strncmp(in, operators[i].strval, len)) {
                *endptr = in + len;
                return operators[i];
            }
        }
        error("Invalid token '%c'.", *in);
    }
    exit(1);
}

struct token get_preprocessing_token(void)
{
    static char *line;

    struct token r;
    char *endptr;

    if (!line && getprepline(&line) == -1) {
        r = end;
    } else {
        r = tokenize(line, &endptr);
        line = endptr;
        if (r.token == END) {
            /* Newlines are removed by getprepline, and never present in the
             * input data. Instead intercept end of string, which represents
             * end of line. */
            line = NULL;
            r = newline;
        }
    }
    return r;
}
