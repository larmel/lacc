#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <stdio.h>

/* Simple numerical value to identify the type of token. Start on 256 to skip
 * the range of ASCII number, which are later explicitly assigned to single-
 * character tokens. This lets us refer to f.ex the PLUS token type as literal
 * '+', which makes the parser more elegant. A sentinel value '$' is used to
 * denote end of file.
 */
struct token {
    enum {
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

        TOKEN_PASTE, /* ## */
        SPACE,

        OR = '|',
        AND = '&',
        XOR = '^',
        MODULO = '%',
        LT = '<',
        GT = '>',
        OPEN_PAREN = '(',
        CLOSE_PAREN = ')',
        COLON = ':',
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
        QUESTION = '?',
        NEG = '~',

        HASH = '#',
        NEWLINE = '\n',

        END = '$'
    } token;

    /* Textual representation of the token as it appears in code. For tokens
     * that are not keywords or operators, the buffer is dynamically allocated.
     */
    const char *strval;

    /* Integer value for constants. */
    long intval;
};

/* Parse and return next preprocessing token from given line. Assumes comments
 * are removed and line continuations are applied. endptr is set to point to one
 * index past the last character producing the token.
 *
 * NB: Destructively overwrites input buffer for string constants.
 */
struct token tokenize(char *in, char **endptr);

/* Define standard macros.
 */
void register_builtin_definitions(void);

/* Lexer exposed to preprocessor.
 */
struct token get_preprocessing_token(void);

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
struct token consume(int type);

/* Output preprocessed input to provided stream, toggled by -E program option.
 */
void preprocess(FILE *output);

#define debug_output_token(t)                                                  \
    do {                                                                       \
        if (t.token == INTEGER_CONSTANT)                                       \
            printf("   token( %ld )\n", t.intval);                             \
        else                                                                   \
            printf("   token( %s )\n", t.strval);                              \
    } while (0);

#endif
