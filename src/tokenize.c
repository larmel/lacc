#include <stdio.h>
#include "lcc.h"

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
                token->value = consumed;
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
        token->value = consumed;
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
        { .value = "auto", .type = AUTO },
        { .value = "break", .type = BREAK },
        { .value = "case", .type = CASE },
        { .value = "char", .type = CHAR },
        { .value = "const", .type = CONST },
        { .value = "continue", .type = CONTINUE },
        { .value = "default", .type = DEFAULT },
        { .value = "do", .type = DO },
        { .value = "double", .type = DOUBLE },
        { .value = "else", .type = ELSE },
        { .value = "enum", .type = ENUM },
        { .value = "extern", .type = EXTERN },
        { .value = "float", .type = FLOAT },
        { .value = "for", .type = FOR },
        { .value = "goto", .type = GOTO },
        { .value = "if", .type = IF },
        { .value = "int", .type = INT },
        { .value = "long", .type = LONG },
        { .value = "register", .type = REGISTER },
        { .value = "return", .type = RETURN },
        { .value = "short", .type = SHORT },
        { .value = "signed", .type = SIGNED },
        { .value = "sizeof", .type = SIZEOF },
        { .value = "static", .type = STATIC },
        { .value = "struct", .type = STRUCT },
        { .value = "switch", .type = SWITCH },
        { .value = "typedef", .type = TYPEDEF },
        { .value = "union", .type = UNION },
        { .value = "unsigned", .type = UNSIGNED },
        { .value = "void", .type = VOID },
        { .value = "volatile", .type = VOLATILE },
        { .value = "while", .type = WHILE }
    };

    if (!isalnum(c)) {
        if (*state == 0)
            *state = 1;
        else 
            *state = -1;
    }

    int i;
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

static void reset();
static int skip_whitespace(FILE *);

/* Get next token from stream, single pass */
int
get_token(FILE *input, struct token *t)
{
    char c;
    int n = 0; /* Number of chars consumed to make token */
    int i;
    int n_matched;

    /* Ignore leading comments and whitespace */
    int whitespace = skip_whitespace(input);

    t->value = NULL;

    /* Simple single char tokens */
    c = fgetc(input);
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
        default:
            ungetc(c, input);
    }

    reset();

    token = t;
    n_matched = -1;

    int s_keyword = 0;
    int s_identifier = 0;
    int s_integer = 0;


    while ((c = fgetc(input)) != EOF) {
        if (keyword(&s_keyword, c)) {
            n_matched = 1;
            ungetc(c, input);
            break;
        }
        if (identifier(&s_identifier, c)) {
            n_matched = 1;
            ungetc(c, input);
            break;
        }
        if (integer(&s_integer, c)) {
            n_matched = 1;
            ungetc(c, input);
            break;
        }

        consumed[n] = c;
        n++;
    }

    printf("\n");

    if (n_matched == -1) {
        printf("Could not match any token for input %s\n", consumed);
        return 0;
    }

    return n;
}

static int
skip_whitespace(FILE *input)
{
    int n = 0;
    char c = fgetc(input);
    while (isspace(c)) {
        c = fgetc(input);
        n++;
    }
    ungetc(c, input);
    return n;
}

static void 
reset()
{
    int i = 0;
    for (i = 0; i < MAX_TOKEN_LENGTH; ++i) {
        consumed[i] = 0;
    }
}
