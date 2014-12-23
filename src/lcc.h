#ifndef LCC_H
#define LCC_H

#include <stdio.h>
#include <stdlib.h>

void init(const char *);

int getprepline(char **);

enum token
{
    AUTO = 256, /* Skip ASCII symbol range to avoid collisions. */
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    ELSE,
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
    VOLATILE,
    WHILE,

    INTEGER,
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

    END = '$' /* Sentinel to mark end of file */
};

typedef struct
{
    enum token type;
    const char *value;
} token_t;

token_t readtoken();
enum token peek();
void consume(enum token expected);

/* Error reporting, functions that can be called from any component */
void error(const char *, ...);


struct function;

/* Parse and compile to intermediate representation */
struct function *parse();

/* Output control flow graph in .dot syntax. */
void fdotgen(FILE *, const struct function *);

/* Output assembly code. */
void fassemble(FILE *, const struct function *);

/* Symbol table. */
void push_scope();
void pop_scope();
void dump_symtab();


#endif
