#include "lcc.h"

#include <ctype.h>
#include <string.h>

#define MAX_TOKEN_LENGTH 256

static char consumed[MAX_TOKEN_LENGTH];

/* Reference to current token structure that should be filled on a match */
static struct token* token;

/* automaton accepting identifiers. return 1 on accept, 0 otherwise */
static int 
identifier(int *state, char c)
{
    switch (*state) {
        case 0:
            if (isalpha(c)) *state = 1;
            else *state = -1;
            break;
        case 1:
            if (isspace(c) || !isalnum(c)) {
                token->type = IDENTIFIER,
                token->value = strdup(consumed);
                *state = 2;
                return 1;
            }
            break;
        default:
            *state = -1;
    }
    return 0;
}

/* automaton accepting integers, return 1 on accept and 0 otherwise */
static int
integer(int *state, char c)
{
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

    int iidx = (c == '0') ? 0 : 
        (c >= '1' && c <= '9') ? 1 :
        ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) ? 2 :
        (c == 'u' || c == 'U') ? 3 :
        (c == 'l' || c == 'L') ? 4 :
        (isspace(c) || !isalnum(c)) ? 5 :
        (c == 'x' || c == 'X') ? 6 : -1;

    *state = t[*state][iidx];
    if (*state / 10) {
        token->type = INTEGER,
        token->value = strdup(consumed);
        return 1;
    }
    return 0;
}

static int
string(int *state, char c)
{
    if (*state == 0) {
        if (c == '"') *state = 1;
        else *state = -1;
    }
    else if (*state == 1) {
        if (c == '\\') *state = 2;
        else if (c == '"') *state = 3;
    }
    else if (*state == 2) {
        *state = 1;
    }

    if (*state == 3) {
        token->type = STRING;
        token->value = strdup(consumed);
        return 1;
    }
    return 0;
}

/* automaton accepting keywords, return 1 on accept and 0 otherwise */
static int
keyword(int *state, char c) 
{
    static struct {
        char * value;
        enum token_type type;
    } keyword[] = {
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
    int i;

    if (!isalnum(c)) {
        if (*state == 0)
            *state = 1;
        else 
            *state = -1;
    }

    if (*state == 1) {
        for (i = 0; i < 32; ++i) {
            if (!strcmp(consumed, keyword[i].value)) {
                token->type = keyword[i].type;
                token->value = keyword[i].value;
                return 1;
            }
        }
    }

    return 0;
}

/* Create tokens from a preprocessed line at a time,
 * no token can span multiple lines. Invoke the preprocessor
 * on demand */
static char *line = "\0";
static int idx = 0;

/* Get next token from stream, single pass */
int
get_token(struct token *t)
{
    char c, d, e;
    int i, n = 0; /* Number of chars consumed to make token */
    int n_matched;
    /*int start, end; */ /* range in current line making up the token */

    /* already failed before */
    if (idx == -1) return 0;

    /* Seek to start of next token */
    do {
        if (line[idx] == '\0') {
            idx = 0;
            if (getprepline(&line) == -1) {
                idx = -1;
                return 0;
            }
        }
        while (isspace(line[idx]))
            idx++;
    } while (line[idx] == '\0');

    t->value = NULL;
    for (i = 0; i < MAX_TOKEN_LENGTH; ++i) {
        consumed[i] = 0;
    }

    /* Simple single char tokens */
    c = line[idx++];
    switch (c) {
        case '(':
            t->type = OPEN_PAREN; t->value = "(";
            return 1;
        case ')':
            t->type = CLOSE_PAREN; t->value = ")";
            return 1;
        case ';':
            t->type = SEMICOLON; t->value = ";";
            return 1;
        case '{':
            t->type = OPEN_CURLY; t->value = "{";
            return 1;
        case '}':
            t->type = CLOSE_CURLY; t->value = "}";
            return 1;
        case '[':
            t->type = OPEN_BRACKET; t->value = "[";
            return 1;
        case ']':
            t->type = CLOSE_BRACKET; t->value = "]";
            return 1;
        case ',':
            t->type = COMMA; t->value = ",";
            return 1;
        case '.':
            d = line[idx++];
            if (d == '.') {
                e = line[idx++];
                if (e == '.') {
                    t->type = DOTS; t->value = "...";
                    return 3;
                } else {
                    error("Unexpected '%c', expected '.'", e);
                    return 0;
                }
            } else {
                idx--;
            }
            t->type = DOT; t->value = ".";
            return 1;
        case '=': /* not exactly right ... */
            t->type = ASSIGN; t->value = "=";
            return 1;
        case '*': /* todo: fix operators such as *= */
            t->type = STAR; t->value = "*";
            return 1;
        default:
            idx--;
    }

    token = t;
    n_matched = -1;

    {
        int s_keyword = 0;
        int s_identifier = 0;
        int s_integer = 0;
        int s_string = 0;

        while ((c = line[idx++]) != '\0') {
            if (keyword(&s_keyword, c)) {
                n_matched = 1;
                idx--;
                break;
            }
            if (identifier(&s_identifier, c)) {
                n_matched = 1;
                idx--;
                break;
            }
            if (integer(&s_integer, c)) {
                n_matched = 1;
                idx--;
                break;
            }
            if (string(&s_string, c)) {
                n_matched = 1;
                consumed[n] = c;
                n++;
                break;
            }

            consumed[n] = c;
            n++;
        }
    }

    if (n_matched == -1 && c != '\0') {
        error("Could not match any token for input '%s'\n", consumed);
        return 0;
    }

    return n;
}
