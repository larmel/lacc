#ifndef TOKEN_H
#define TOKEN_H

#include "string.h"
#include "type.h"

/*
 * Map token type to corresponding numerical ascii value where possible,
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
    INLINE,

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

    /*
     * The remaining tokens do not correspond to any fixed string, and
     * are placed at arbitrary locations.
     */
    NUMBER = 116,
    IDENTIFIER,
    STRING,

    /*
     * Pseudo-token representing parameter substitution in macros. Has
     * an immediate integer value referring to the parameter index.
     */
    PARAM,

    /*
     * Pseudo-token representing empty argument for macro expansion in
     * preprocessor.
     */
    EMPTY_ARG,

    /*
     * Preprocessing token representing a number. Valid strings include
     * a superset of numeric constants in C. A conversion to NUMBER
     * token is done before handed to parser.
     */
    PREP_NUMBER
};

/*
 * Tokens keep track of typed numbers, to capture difference between
 * literals like 1 and 1ul. Type should always correspond to one of the
 * basic integer types.
 *
 * The value union is also used in internal representation of immediate
 * numeric values.
 */
struct number {
    Type type;
    union value {
        unsigned long u;
        signed long i;
        float f;
        double d;
        long double ld;
    } val;
};

struct number convert(struct number num, Type type);

/*
 * Representation of token; used in preprocessing, and interface to
 * parser.
 */
struct token {
    enum token_type token;
    unsigned int leading_whitespace : 16;
    unsigned int is_expandable : 1;
    unsigned int is_char_literal : 1;
    unsigned int disable_expand : 1;
    union {
        String string;
        struct number number;
    } d;
};

/* Peek lookahead of 1. */
struct token peek(void);

/* Peek lookahead of n. */
struct token peekn(int n);

/* Consume and return next token. */
struct token next(void);

/* Consume and return next token, or fail of not of expected type. */
struct token consume(enum token_type type);

#endif
