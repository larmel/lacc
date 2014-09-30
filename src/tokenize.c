#include "lcc.h"

#include <ctype.h>
#include <string.h>


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
        if (state == 2) return read;
        if (state < 0) return 0;
        read++;
    }
}

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
        if (state / 10) return read;
        if (state < 0) return 0;
        read++;
    }
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
        if (state == 3) return read;
        if (state < 0) return 0;
    }
}

static struct {
    char * value;
    enum token_type type;
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

int
get_token(struct token *t)
{
    int n;

    /* Create tokens from a preprocessed line at a time, no token can span
     * multiple lines. Invoke the preprocessor on demand */
    static char *line;

    /* Current strtok token, preserved across invocations */
    static char *tok = NULL;

    /* Get next strtok of current preprocessed line */
    if (tok != NULL && *tok == '\0')
        tok = strtok(NULL, " \t\n");

    /* Need more stuff from preprocessor */
    if (tok == NULL) {
        if (getprepline(&line) == -1) {
            return 0; /* eof */
        }
        tok = strtok(line, " \t\n");
    }

    for (n = 0; n < 32; ++n) {
        int length = strlen(keywords[n].value);
        if (!strncmp(tok, keywords[n].value, length) && !isalnum(*(tok + length))) {
            tok += length;
            t->type = keywords[n].type;
            t->value = keywords[n].value;
            return length;
        }
    }

    switch (*tok++) {
        case '+':
            if (*tok == '+') {
                tok++;
                t->type = INCREMENT; t->value = "++";
                return 2;
            }
            t->type = PLUS; t->value = "+";
            return 1;
        case '-':
            if (*tok == '-') {
                tok++;
                t->type = DECREMENT; t->value = "--";
                return 2;
            }
            if (*tok == '>') {
                tok++;
                t->type = ARROW; t->value = "->";
                return 2;
            }
            t->type = MINUS; t->value = "-";
            return 1;
        case '!':
            if (*tok == '=') {
                tok++;
                t->type = NEQ; t->value = "!=";
                return 2;
            }
            t->type = NOT; t->value = "!";
            return 1;
        case '|':
            if (*tok == '|') {
                tok++;
                t->type = LOGICAL_OR; t->value = "||";
                return 2;
            }
            t->type = OR; t->value = "|";
            return 1;
        case '&':
            if (*tok == '&') {
                tok++;
                t->type = LOGICAL_AND; t->value = "&&";
                return 2;
            }
            t->type = AND; t->value = "&";
            return 1;
        case '^':
            t->type = XOR; t->value = "^";
            return 1;
        case '%':
            t->type = MODULO; t->value = "%%";
            return 1;
        case '<':
            if (*tok == '=') {
                tok++;
                t->type = LEQ; t->value = "<=";
                return 2;
            }
            t->type = LT; t->value = "<";
            return 1;
        case '>':
            if (*tok == '=') {
                tok++;
                t->type = GEQ; t->value = ">=";
                return 2;
            }
            t->type = GT; t->value = ">";
            return 1;
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
            if (strncmp(tok, "..", 2)) {
                tok += 2;
                t->type = DOTS; t->value = "...";
                return 3;
            }
            t->type = DOT; t->value = ".";
            return 1;
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
                t->type = INTEGER;
                t->value = strndup(tok - 1, n);
                tok += n - 1;
                return n;
            }
            break;
        case '"':
            n = string(tok - 1);
            if (n) {
                t->type = STRING;
                t->value = strndup(tok, n - 1);
                tok += n - 1;
                return n;
            }
            break;
        case '=':
            if (*tok == '=') {
                tok++;
                t->type = EQ; t->value = "==";
                return 2;
            }
            t->type = ASSIGN; t->value = "=";
            return 1;
        case '*': /* todo: fix operators such as *= */
            t->type = STAR; t->value = "*";
            return 1;
        case '/':
            t->type = SLASH; t->value = "/";
            return 1;
        default:
            n = identifier(tok - 1);
            if (n) {
                t->type = IDENTIFIER;
                t->value = strndup(tok - 1, n);
                tok += n - 1;
                return n;
            }
            break;
    }
    tok--;

    error("Could not match any token for input '%s'\n", tok);
    return 0;
}
