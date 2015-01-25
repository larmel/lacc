#include "error.h"
#include "token.h"

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Import interface from preprocessor. */
extern int getprepline(char **);


/* Store previous identifier. The C standard specifies a fixed limit, which
 * makes it possible to store this in a statically allocated buffer. */
static char ident[64];

static int identifier(char *input)
{
    int state = 0, read = 0;
    while (1) {
        char c = *input++;
        switch (state) {
            case 0:
                if (isalpha(c)) state = 1;
                else state = -1;
                break;
            case 1:
                if (isspace(c) || !isalnum(c)) {
                    state = 2;
                }
                break;
            default:
                state = -1;
        }
        if (state < 0) return 0;
        if (state == 2) {
            ident[read] = '\0';
            return read;
        }
        if (read > 63) {
            error("Illegal identifier length, exceeding 63 character limit.");
            return 0;
        }

        ident[read++] = c;
    }
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

static char escpchar(char *p, char **endptr)
{
    *endptr = p + 1;
    if (*p != '\\') return *p;

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
            return 0;
        case 'x':
            return (char) strtol(&p[2], endptr, 16);
        default:
            *endptr = p;
            return 0;
    }
    return 0;
}

/* Parse character literals in the format 'a', '\xaf', '\0', '\077' etc,
 * starting from *in. The position of the character after the last ' character
 * is stored in endptr. If no valid conversion can be made, *endptr == in.
 */
static char strtochar(char *in, char **endptr)
{
    char value, *end;

    *endptr = in;
    if (*in++ != '\'') return 0;

    value = escpchar(in, &end);
    if (end == in) return 0;

    if (*in++ != '\'') return 0;

    *endptr = in;
    return value;
}

/* Parse string literal inputs delimited by quotation marks, handling escaped
 * quotes. The input buffer is destructively overwritten while resolving escape
 * sequences. */
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
 * tokens and match using simple string comparison. */
static enum token
get_token()
{
    static char *line;
    static char *tok = NULL;

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
        { "do", DO },
        { "double", DOUBLE },
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

    if (tok == NULL || *tok == '\0') {
        if (getprepline(&line) == -1) {
            return END;
        }
        tok = line;
    }
    while (isspace(*tok))
        tok++;

    for (n = 0; n < 55; ++n) {
        int length = strlen(reserved[n].value);
        if (!strncmp(tok, reserved[n].value, length)) {
            if (n < 32 && isalnum(tok[length]))
                break;
            tok += length;
            tok_strval = reserved[n].value;
            return reserved[n].type;
        }
    }

    if (isalpha(*tok) || *tok == '_') {
        n = identifier(tok);
        if (n) {
            tok_strval = ident;
            tok += n;
            return IDENTIFIER;
        }
        error("Invalid identifier %s.", tok);
        exit(1);
    }

    if (isdigit(*tok)) {
        tok_intval = strtonum(tok, &end);
        if (end != tok) {
            tok = end;
            return INTEGER_CONSTANT;
        }
        error("Invalid number literal %s.", tok);
        exit(1);
    }

    switch (*tok) {
        case '"':
            tok_strval = strtostr(tok, &end);
            if (end != tok) {
                tok = end;
                return STRING;
            }
            error("Invalid string literal %s.", tok);
            exit(1);
        case '\'':
            tok_intval = strtochar(tok, &end);
            if (end != tok) {
                tok = end;
                return INTEGER_CONSTANT;
            }
            error("Invalid character literal %s.", tok);
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

long tok_intval;
const char *tok_strval;

enum token readtoken() {
    if (has_value) {
        if (peek_value != END)
            has_value = 0;
        return peek_value;
    }
    return get_token();
}

enum token peek() {
    if (!has_value) {
        peek_value = readtoken();
        has_value = 1;
    }
    return peek_value;
}

void consume(enum token expected) {
    enum token t = readtoken();
    if (t != expected) {
        if (isprint((int) t) && isprint((int) expected))
            error("Unexpected token `%c`, expected `%c`", (char) t, (char) expected);
        else if (isprint((int) t))
            error("Unexpected token `%c`", (char) t);
        else 
            error("Unexpected token");
        exit(1);
    }
}
