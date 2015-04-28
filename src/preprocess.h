#ifndef PREPROCESS_H
#define PREPROCESS_H

/* Simple numerical value to identify the type of token. Start on 256 to skip
 * the range of ASCII number, which are later explicitly assigned to single-
 * character tokens. This lets us refer to f.ex the PLUS token type as literal
 * '+', which makes the parser more elegant. 
 * A sentinel value '$' is used to denote end of file. */
enum token_type
{
    AUTO = 256, BREAK, CASE, CHAR,
    CONST, CONTINUE, DEFAULT, DO,
    DOUBLE, ELSE, ENUM, EXTERN,
    FLOAT, FOR, GOTO, IF,
    INT, LONG, REGISTER, RETURN,
    SHORT, SIGNED, SIZEOF, STATIC,
    STRUCT, SWITCH, TYPEDEF, UNION,
    UNSIGNED, VOID, VOLATILE, WHILE,

    INTEGER_CONSTANT,
    IDENTIFIER,
    STRING,

    DOTS, /* ... */
    LOGICAL_OR, /* || */
    LOGICAL_AND, /* && */
    LEQ, /* <= */
    GEQ, /* >= */
    EQ, /* == */
    NEQ, /* != */
    ARROW, /* -> */
    INCREMENT, /* ++ */
    DECREMENT, /* -- */
    LSHIFT, /* << */
    RSHIFT, /* >> */

    MUL_ASSIGN, /* *= */
    DIV_ASSIGN, /* /= */
    MOD_ASSIGN, /* %= */
    PLUS_ASSIGN, /* += */
    MINUS_ASSIGN, /* -= */
    LSHIFT_ASSIGN, /* <<= */
    RSHIFT_ASSIGN, /* >>= */
    AND_ASSIGN, /* &= */
    XOR_ASSIGN, /* ^= */
    OR_ASSIGN, /* |= */

    OR = '|',
    AND = '&',
    XOR = '^',
    MODULO = '%',
    LT = '<',
    GT = '>',
    OPEN_PAREN = '(',
    CLOSE_PAREN = ')',
    SEMICOLON = ';',
    OPEN_CURLY = '{',
    CLOSE_CURLY = '}',
    OPEN_BRACKET = '[',
    CLOSE_BRACKET = ']',
    COMMA = ',',
    DOT = '.',
    ASSIGN = '=',
    STAR = '*',
    SLASH = '/',
    PLUS = '+',
    MINUS = '-',
    NOT = '!',
    NEG = '~',

    HASH = '#',
    NEWLINE = '\n',
    SPACE = ' ',

    END = '$'
};

struct token {
    enum token_type token;
    const char *strval;
    long intval;
};

#define debug_output_token(t) \
    do {\
        if (t.token == INTEGER_CONSTANT) \
            printf("   token( %ld )\n", t.intval); \
        else \
            printf("   token( %s )\n", t.strval); \
    } while (0);

/* Define standard macros. */
void register_builtin_definitions();

/* Lexer exposed to preprocessor. */
struct token next_raw_token();
enum token_type peek_raw_token();
void consume_raw_token(enum token_type);

/* Preprocessor interface exposed to the parser, return tokens where any macro 
 * substitution or preprocessing directives have been handled. */
struct token next();
struct token peek();
struct token peekn(unsigned);
struct token consume(enum token_type);

#endif
