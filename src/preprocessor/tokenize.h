#ifndef TOKENIZE_H
#define TOKENIZE_H

/* Map token type to corresponding numerical ascii value where possible,
 * and fit the remaining tokens in between.
 */
enum token_type
{
    END = 0,                /*  $ */
    AUTO = END + 1,
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    NEWLINE = '\n',
    ELSE = NEWLINE + 1,
    ENUM,
    EXTERN,
    FLOAT,
    FOR,
    GOTO,
    IF,
    INT,
    LONG,
    REGISTER,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STRUCT,
    SWITCH,
    TYPEDEF,
    UNION,
    UNSIGNED,
    VOID,
    SPACE = ' ',
    NOT = '!',
    VOLATILE = NOT + 1,
    HASH = '#',
    WHILE = HASH + 1,
    MODULO = '%',
    AND = '&',

    OPEN_PAREN = '(',
    CLOSE_PAREN = ')',
    STAR = '*',
    PLUS = '+',
    COMMA = ',',
    MINUS = '-',
    DOT = '.',
    SLASH = '/',

    COLON = ':',
    SEMICOLON = ';',
    LT = '<',
    ASSIGN = '=',
    GT = '>',
    QUESTION = '?',
    DOTS = QUESTION + 1,    /* ... */
    LOGICAL_OR,             /* || */
    LOGICAL_AND,            /* && */
    LEQ,                    /* <= */
    GEQ,                    /* >= */
    EQ,                     /* == */
    NEQ,                    /* != */
    ARROW,                  /* -> */
    INCREMENT,              /* ++ */
    DECREMENT,              /* -- */
    LSHIFT,                 /* << */
    RSHIFT,                 /* >> */
    MUL_ASSIGN,             /* *= */
    DIV_ASSIGN,             /* /= */
    MOD_ASSIGN,             /* %= */
    PLUS_ASSIGN,            /* += */
    MINUS_ASSIGN,           /* -= */
    LSHIFT_ASSIGN,          /* <<= */
    RSHIFT_ASSIGN,          /* >>= */
    AND_ASSIGN,             /* &= */
    XOR_ASSIGN,             /* ^= */
    OR_ASSIGN,              /* |= */
    TOKEN_PASTE,            /* ## */

    OPEN_BRACKET = '[',
    CLOSE_BRACKET = ']',
    XOR = '^',
    OPEN_CURLY = '{',
    OR = '|',
    CLOSE_CURLY = '}',
    NEG = '~',

    /* The remaining tokens do not correspond to any fixed string, and
     * are placed at arbitrary locations. */
    INTEGER_CONSTANT = 116,
    IDENTIFIER,
    STRING
};

/* Every token has a reference to its textual value. The integer value is
 * kept for numeric constants.
 */
struct token
{
    enum token_type token;
    const char *strval;
    long intval;
};

/* Mapping to ASCII indexed token strings.
 */
extern const char *reserved[128];

/* Parse and return next preprocessing token from given line. Assume comments
 * are removed and line continuations are applied. endptr is set to point to
 * one index past the last character producing the token.
 *
 * Destructively overwrites input buffer for string constants.
 */
struct token tokenize(char *in, char **endptr);

/* Global instances of tokens representing end of input, and end of line,
 * respectively.
 */
extern struct token
    token_end,
    token_newline;

#endif
