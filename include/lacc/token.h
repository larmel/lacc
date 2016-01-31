#ifndef TOKEN_H
#define TOKEN_H

#include "string.h"

/* Map token type to corresponding numerical ascii value where possible,
 * and fit the remaining tokens in between.
 */
enum token_type {
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
struct token {
    enum token_type token;
    struct string strval;
    long intval;
};

/* Peek lookahead of 1.
 */
struct token peek(void);

/* Peek lookahead of n.
 */
struct token peekn(unsigned n);

/* Consume and return next token.
 */
struct token next(void);

/* Consume and return next token, or fail of not of expected type.
 */
struct token consume(enum token_type type);

#endif
