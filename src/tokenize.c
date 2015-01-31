#include "token.h"
#include "error.h"

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Import interface from preprocessor. */
extern int getprepline(char **);


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

/* Parse and return next token from line buffer yielded by the preprocessor.
 * Create tokens from a preprocessed line at a time, no token can span multiple
 * lines. Invoke the preprocessor on demand. Iterate over all multi-character 
 * tokens and match using simple string comparison.
 */
static enum token
get_token()
{
    static char *tok;

    static struct {
        const char *value;
        enum token type;
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
        { "%%=", MOD_ASSIGN },
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
        { ">>", RSHIFT }, /* 54 */
    };

    int n;
    char *end;

    while (tok && isspace(*tok))
        tok++;

    if (tok == NULL || *tok == '\0')
        if (getprepline(&tok) == -1)
            return END;

    for (n = 0; n < 55; ++n) {
        int length = strlen(reserved[n].value);
        if (!strncmp(tok, reserved[n].value, length)) {
            if (n < 32 && isalnum(tok[length]))
                break;
            tok += length;
            strval = reserved[n].value;
            return reserved[n].type;
        }
    }

    if (isalpha(*tok) || *tok == '_') {
        strval = strtoident(tok, &end);
        if (end != tok) {
            tok = end;
            return IDENTIFIER;
        }
        error("Invalid identifier: `%s`.", tok);
        exit(1);
    }

    if (isdigit(*tok)) {
        intval = strtonum(tok, &end);
        if (end != tok) {
            tok = end;
            return INTEGER_CONSTANT;
        }
        error("Invalid number literal: `%s`.", tok);
        exit(1);
    }

    switch (*tok) {
        case '"':
            strval = strtostr(tok, &end);
            if (end != tok) {
                tok = end;
                return STRING;
            }
            error("Invalid string literal: `%s`.", tok);
            exit(1);
        case '\'':
            intval = strtochar(tok, &end);
            if (end != tok) {
                tok = end;
                return INTEGER_CONSTANT;
            }
            error("Invalid character literal: `%s`.", tok);
            exit(1);
        default:
            break;
    }

    return *tok++;
}

static enum token peek_value;
static int has_value;

/* 
 * External interface.
 */

long intval;

const char *strval;

enum token token() {
    if (has_value) {
        if (peek_value != END)
            has_value = 0;
        return peek_value;
    }
    return get_token();
}

enum token peek() {
    if (!has_value) {
        peek_value = token();
        has_value = 1;
    }
    return peek_value;
}

void consume(enum token expected) {
    enum token t = token();
    if (t != expected) {
        if (isprint(t) && isprint(expected))
            error("Unexpected token `%c`, expected `%c`.", t, expected);
        else if (isprint(t))
            error("Unexpected token `%c`.", t);
        else 
            error("Unexpected token `%s`.", strval);
        exit(1);
    }
}
