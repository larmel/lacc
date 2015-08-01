#if _XOPEN_SOURCE < 700
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strdup, strndup */
#endif
#include "error.h"
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
    }

    if (endptr)
        *endptr = end;

    return value;
}

/* Parse character escape code, including octal and hexadecimal number
 * literals. Unescaped characters are returned as-is. Invalid escape sequences
 * continues with an error, consuming only the backslash.
 */
static char escpchar(char *p, char **endptr)
{
    if (*p == '\\') {
        *endptr = p + 2;
        switch (p[1]) {
            case 'a': return 0x7;
            case 'b': return 0x8;
            case 't': return 0x9;
            case 'n': return 0xa;
            case 'v': return 0xb;
            case 'f': return 0xc;
            case 'r': return 0xd;
            case '\\': return '\\';
            case '?': return '\?';
            case '`': return '`';
            case '\"': return '\"';
            case '0':
                if (isdigit(p[2]) && p[2] < '8')
                    return (char) strtol(&p[1], endptr, 8);
                return '\0';
            case 'x':
                return (char) strtol(&p[2], endptr, 16);
            default:
                error("Invalid escape sequence `\\%c`.", p[1]);
        }
    }

    *endptr = p + 1;
    return *p;
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
 * sequences.
 */
static const char *strtostr(char *in, char **endptr)
{
    char *start, *str;

    start = str = in;
    *endptr = in;

    if (*in++ == '"') {
        while (*in != '"' && *in) {
            *str++ = escpchar(in, &in);
        }

        if (*in++ == '"') {
            *str = '\0';
            *endptr = in;
        }
    }

    return start;
}

/* Parse and return next preprocessing token, from char buffer where comments
 * are removed and line continuations are applied.
 */
struct token tokenize(char *in, char **endptr)
{
    static struct {
        const char *value;
        enum token_type type;
    } reserved[] = {
        { "auto", AUTO },
        { "break", BREAK },
        { "case", CASE },
        { "char", CHAR },
        { "const", CONST },
        { "continue", CONTINUE },
        { "default", DEFAULT },
        { "double", DOUBLE },
        { "do", DO },
        { "else", ELSE },
        { "enum", ENUM },
        { "extern", EXTERN },
        { "float", FLOAT },
        { "for", FOR },
        { "goto", GOTO },
        { "if", IF },
        { "int", INT },
        { "long", LONG },
        { "register", REGISTER },
        { "return", RETURN },
        { "short", SHORT },
        { "signed", SIGNED },
        { "sizeof", SIZEOF },
        { "static", STATIC },
        { "struct", STRUCT },
        { "switch", SWITCH },
        { "typedef", TYPEDEF },
        { "union", UNION },
        { "unsigned", UNSIGNED },
        { "void", VOID },
        { "volatile", VOLATILE },
        { "while", WHILE }, /* 31 */

        { "*=", MUL_ASSIGN },
        { "/=", DIV_ASSIGN },
        { "\x25=", MOD_ASSIGN },
        { "+=", PLUS_ASSIGN },
        { "-=", MINUS_ASSIGN },
        { "<<=", LSHIFT_ASSIGN },
        { ">>=", RSHIFT_ASSIGN },
        { "&=", AND_ASSIGN },
        { "^=", XOR_ASSIGN },
        { "|=", OR_ASSIGN },
        { "...", DOTS },
        { "||", LOGICAL_OR },
        { "&&", LOGICAL_AND },
        { "<=", LEQ },
        { ">=", GEQ },
        { "==", EQ },
        { "!=", NEQ },
        { "->", ARROW },
        { "++", INCREMENT },
        { "--", DECREMENT },
        { "<<", LSHIFT },
        { ">>", RSHIFT },

        { "##", TOKEN_PASTE } /* 55 */
    };

    int n;
    struct token res = {END, NULL, 0};

    assert(endptr);
    assert(in && *in != '\0');

    if (isspace(*in)) {
        res.token = SPACE;
        res.strval = " ";
        while (isspace(*in)) {
            in++;
            res.intval++;
        }
        *endptr = in;
        return res;
    }

    for (n = 0; n < 55; ++n) {
        int length = strlen(reserved[n].value);
        if (!strncmp(in, reserved[n].value, length)) {
            if (n < 32 && (isalnum(in[length]) || in[length] == '_')) {
                break;
            }
            res.token = reserved[n].type;
            res.strval = reserved[n].value;
            *endptr = in + length;
            return res;
        }
    }

    if (isalpha(*in) || *in == '_') {
        res.strval = strtoident(in, endptr);
        if (*endptr != in) {
            res.token = IDENTIFIER;
            res.strval = strdup(res.strval);
            return res;
        }
        error("Invalid identifier: `%s`.", in);
        exit(1);
    }

    if (isdigit(*in)) {
        res.intval = strtonum(in, endptr);
        if (*endptr != in) {
            res.token = INTEGER_CONSTANT;
            res.strval = strndup(in, *endptr - in);
            return res;
        }
        error("Invalid number literal: `%s`.", in);
        exit(1);
    }

    switch (*in) {
        case '"':
            res.strval = strtostr(in, endptr);
            if (*endptr != in) {
                res.token = STRING;
                res.strval = strdup(res.strval);
                return res;
            }
            error("Invalid string literal: `%s`.", in);
            exit(1);
        case '\'':
            res.intval = strtochar(in, endptr);
            if (*endptr != in) {
                res.token = INTEGER_CONSTANT;
                res.strval = strndup(in, *endptr - in);
                return res;
            }
            error("Invalid character literal: `%s`.", in);
            exit(1);
        case '|': res.strval = "|"; break;
        case '&': res.strval = "&"; break;
        case '^': res.strval = "^"; break;
        case '%': res.strval = "%%"; break;
        case '<': res.strval = "<"; break;
        case '>': res.strval = ">"; break;
        case '(': res.strval = "("; break;
        case ')': res.strval = ")"; break;
        case ';': res.strval = ";"; break;
        case ':': res.strval = ":"; break;
        case '{': res.strval = "{"; break;
        case '}': res.strval = "}"; break;
        case '[': res.strval = "["; break;
        case ']': res.strval = "]"; break;
        case ',': res.strval = ","; break;
        case '.': res.strval = "."; break;
        case '=': res.strval = "="; break;
        case '*': res.strval = "*"; break;
        case '/': res.strval = "/"; break;
        case '+': res.strval = "+"; break;
        case '-': res.strval = "-"; break;
        case '!': res.strval = "!"; break;
        case '?': res.strval = "?"; break;
        case '~': res.strval = "~"; break;
        case '#': res.strval = "#"; break;
        default:
            error("Invalid token '%c'.", *in);
            exit(1);
    }

    res.token = *in++;
    *endptr = in;
    return res;
}

/* Hold current clean line to be tokenized. */
char *line;

/* Use one lookahead for preprocessing token. */
static struct token prep_token_peek;
static int has_prep_token_peek;

struct token next_raw_token()
{
    static struct token
        tok_end = {END, NULL, 0},
        tok_nl = {NEWLINE, NULL, 0};

    struct token r;
    char *end;

    if (has_prep_token_peek) {
        has_prep_token_peek = 0;
        return prep_token_peek;
    }

    do {
        if (!line && getprepline(&line) == -1) {
            r = tok_end;
        } else if (*line == '\0') {
            line = NULL;
            r = tok_nl;
        } else {
            r = tokenize(line, &end);
            line = end;
        }

    } while (r.token == SPACE);
    /*debug_output_token(r); */
    return r;
}

enum token_type peek_raw_token()
{
    if (has_prep_token_peek) {
        return prep_token_peek.token;
    }

    prep_token_peek = next_raw_token();
    has_prep_token_peek = 1;

    /*debug_output_token(prep_token_peek); */

    return prep_token_peek.token;
}

void consume_raw_token(enum token_type t)
{
    struct token read = next_raw_token();

    if (read.token != t) {
        error("Unexpected preprocessing token.");
        printf("  -> Token was:");
        debug_output_token(read);
        if (isprint((int) t)) {
            printf("  -> Expected %c\n", (char) t);
        } else {
            printf("  -> Expected %d\n", (int) t);
        }
        exit(1);
    }
}
