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

/* Store character representation of numeric constant. Better not be larger than
 * this. */
static char number[64];

static int integer(char *input)
{
    int state = 0, read = 0;
    static char t[7][7] = {
        /* 0 | 1-9 | a-fA-F | uU | lL | _ | xX */
        {  1,   2,     -1,    -1,  -1, -1,  -1 },
        {  2,   2,     -1,     5,  -1, 10,   3 },
        {  2,   2,     -1,     5,   6, 20,  -1 },
        {  4,   4,      4,    -1,  -1, -1,  -1 },
        {  4,   4,      4,     5,   6, 40,  -1 },
        { -1,  -1,     -1,    -1,   6, 50,  -1 },
        { -1,  -1,     -1,    -1,  -1, 60,  -1 }
    };

    while (1) {
        char c = *input++;
        int i = (c == '0') ? 0 : 
            (c >= '1' && c <= '9') ? 1 :
            ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) ? 2 :
            (c == 'u' || c == 'U') ? 3 :
            (c == 'l' || c == 'L') ? 4 :
            (isspace(c) || !isalnum(c)) ? 5 :
            (c == 'x' || c == 'X') ? 6 : -1;
        state = t[state][i];
        if (state < 0) return 0;
        if (state / 10) {
            number[read] = '\0';
            return read;
        }
        if (read > 63) {
            error("Number literal too long.");
            return 0;
        }
        number[read++] = c;
    }
}

/* Parse character literals in the format 'a', '\xaf', '\0', '\077' etc,
 * starting from *in. The position of the character after the last ' character
 * is stored in endptr. If no valid conversion can be made, *endptr == in.
 */
static char strtochar(char *in, char **endptr)
{
    char value, *end;

    if (endptr)
        *endptr = in;

    if (*in++ != '\'')
        return 0;

    value = *in++;
    if (value == '\\') {
        switch (*in++) {
            case 'a': value = 0x7; break;
            case 'b': value = 0x8; break;
            case 't': value = 0x9; break;
            case 'n': value = 0xa; break;
            case 'v': value = 0xb; break;
            case 'f': value = 0xc; break;
            case 'r': value = 0xd; break;
            case '\\': value = '\\'; break;
            case '?': value = '\?'; break;
            case '`': value = '`'; break;
            case '\"': value = '\"'; break;
            case '0': 
                if (*in == '\'') {
                    value = 0; 
                    break;
                }
                value = (char) strtol(in, &end, 8);
                if (end == in) {
                    error("Invalid octal literal.");
                    return 0;
                }
                in = end;
                break;
            case 'x':
                value = (char) strtol(in, &end, 16);
                if (end == in) {
                    error("Invalid hex literal.");
                    return 0;
                }
                in = end;
                break;
            default:
                error("Invalid escape sequence.");
                return 0;
        }
    }
    if (*in++ != '\'')
        return 0;

    if (endptr)
        *endptr = in;
    return value;
}

static int string(char *input)
{
    int state = 0, read = 0;
    while (1) {
        char c = *input++;
        if (state == 0) {
            if (c == '"') state = 1;
            else state = -1;
        }
        else if (state == 1) {
            if (c == '\\') state = 2;
            else if (c == '"') state = 3;
        }
        else if (state == 2) {
            state = 1;
        }
        read++;
        if (state == 3) {
            return read;
        }
        if (state < 0) return 0;
    }
}

static struct {
    char * value;
    enum token type;
} keywords[] = {
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
    { "while", WHILE }
};

static token_t
get_token()
{
    int n;
    token_t token;

    /* Create tokens from a preprocessed line at a time, no token can span
     * multiple lines. Invoke the preprocessor on demand */
    static char *line;

    /* Current start of token in preprocessed line. */
    static char *tok = NULL, *end;

    /* Need more stuff from preprocessor */
    if (tok == NULL || *tok == '\0') {
        if (getprepline(&line) == -1) {
            token.type = END;
            token.value.string = NULL;
            return token;
        }
        tok = line;
    }

    while (isspace(*tok))
        tok++;

    for (n = 0; n < 32; ++n) {
        int length = strlen(keywords[n].value);
        if (!strncmp(tok, keywords[n].value, length) && !isalnum(*(tok + length))) {
            tok += length;
            token.type = keywords[n].type;
            token.value.string = keywords[n].value;
            return token;
        }
    }

    switch (*tok++) {
        case '+':
            if (*tok == '+') {
                tok++;
                token.type = INCREMENT; token.value.string = "++";
                return token;
            }
            token.type = PLUS; token.value.string = "+";
            return token;
        case '-':
            if (*tok == '-') {
                tok++;
                token.type = DECREMENT; token.value.string = "--";
                return token;
            }
            if (*tok == '>') {
                tok++;
                token.type = ARROW; token.value.string = "->";
                return token;
            }
            token.type = MINUS; token.value.string = "-";
            return token;
        case '!':
            if (*tok == '=') {
                tok++;
                token.type = NEQ; token.value.string = "!=";
                return token;
            }
            token.type = NOT; token.value.string = "!";
            return token;
        case '|':
            if (*tok == '|') {
                tok++;
                token.type = LOGICAL_OR; token.value.string = "||";
                return token;
            }
            token.type = OR; token.value.string = "|";
            return token;
        case '&':
            if (*tok == '&') {
                tok++;
                token.type = LOGICAL_AND; token.value.string = "&&";
                return token;
            }
            token.type = AND; token.value.string = "&";
            return token;
        case '^':
            token.type = XOR; token.value.string = "^";
            return token;
        case '%':
            token.type = MODULO; token.value.string = "%%";
            return token;
        case '<':
            if (*tok == '=') {
                tok++;
                token.type = LEQ; token.value.string = "<=";
                return token;
            }
            token.type = LT; token.value.string = "<";
            return token;
        case '>':
            if (*tok == '=') {
                tok++;
                token.type = GEQ; token.value.string = ">=";
                return token;
            }
            token.type = GT; token.value.string = ">";
            return token;
        case '(':
            token.type = OPEN_PAREN; token.value.string = "(";
            return token;
        case ')':
            token.type = CLOSE_PAREN; token.value.string = ")";
            return token;
        case ';':
            token.type = SEMICOLON; token.value.string = ";";
            return token;
        case '{':
            token.type = OPEN_CURLY; token.value.string = "{";
            return token;
        case '}':
            token.type = CLOSE_CURLY; token.value.string = "}";
            return token;
        case '[':
            token.type = OPEN_BRACKET; token.value.string = "[";
            return token;
        case ']':
            token.type = CLOSE_BRACKET; token.value.string = "]";
            return token;
        case ',':
            token.type = COMMA; token.value.string = ",";
            return token;
        case '.':
            if (strncmp(tok, "..", 2)) {
                tok += 2;
                token.type = DOTS; token.value.string = "...";
                return token;
            }
            token.type = DOT; token.value.string = ".";
            return token;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            n = integer(tok - 1);
            if (n) {
                token.type = INTEGER_CONSTANT;
                token.value.integer = (long)number;
                tok += n - 1;
                return token;
            }
            break;
        case '"':
            n = string(tok - 1);
            if (n) {
                /* Overwrite the last " to create end of string. */
                tok[n - 2] = '\0';
                token.type = STRING;
                token.value.string = tok;
                tok += n - 1;
                return token;
            }
            break;
        case '\'':
            token.type = INTEGER_CONSTANT;
            token.value.integer = strtochar(tok - 1, &end);
            if (end != tok - 1) {
                tok = end;
                return token;
            }
            break;
        case '=':
            if (*tok == '=') {
                tok++;
                token.type = EQ; token.value.string = "==";
                return token;
            }
            token.type = ASSIGN; token.value.string = "=";
            return token;
        case '*': /* todo: fix operators such as *= */
            token.type = STAR; token.value.string = "*";
            return token;
        case '/':
            token.type = SLASH; token.value.string = "/";
            return token;
        default:
            n = identifier(tok - 1);
            if (n) {
                token.type = IDENTIFIER;
                token.value.string = ident;
                tok += n - 1;
                return token;
            }
            break;
    }
    error("Could not match any token for input `%s`", tok);
    exit(1);
}

static token_t peek_value;
static int has_value;

/* Tokenization interface. */
token_t
readtoken()
{
    token_t t;
    if (has_value) {
        if (peek_value.type != END)
            has_value = 0;
        return peek_value;
    }
    t = get_token();
    return t;
}

enum token
peek()
{
    if (!has_value) {
        peek_value = readtoken();
        has_value = 1;
    }
    return peek_value.type;
}

void
consume(enum token expected)
{
    token_t t = readtoken();
    if (t.type != expected) {
        error("Unexpected %s, aborting.", (t.type == '$') ?
            "end of file" : "token", t.value);
        exit(1);
    }
}
