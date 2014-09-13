#ifndef LCC_H
#define LCC_H

#include <stdio.h>
#include <stdlib.h>

void init_preprocessing(const char *);
int getprepline(char **);

typedef enum token_type
{
    /* Keyword */
    AUTO,
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

    /* single character */
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
} token_t;

struct token
{
    enum token_type type;
    /* Depending on token type, store a reference to an integer, char, string,
     * or other compile time constant. */
    void *value;
};

int get_token(FILE *input, struct token* t);


/* parsing */

typedef struct node {
    const char *text;
    struct token token;
    struct node **children;
    size_t nc;
    size_t cap;
} node_t;

struct node * parse(FILE *);


/* type analysis */

enum type { POINTER, T_INT, T_LONG, T_FLOAT, T_DOUBLE, T_CHAR, FUNCTION };

typedef struct typestack {
    enum type type;

    /* if pointer */
    struct typestack *ptrto;

    /* if function */
    struct typestack **args;
    unsigned n_args;
    struct typestack *retval;
} tstack_t;

/* int *foo;
 * ( .name = "foo", .type )
 *                     |
 *          ( .type = POINTER, .ptrto )
 *                               |
 *                       ( .type = T_INT )
 */
typedef struct symbol {
    const char *name;
    unsigned depth;
    struct typestack *type;
} symbol_t;

/* resolve symbol in current scope, or NULL if not found */
symbol_t *sym_lookup(const char *);

/* add symbol to current scope */
symbol_t *sym_add(const char *);

void push_scope();

void pop_scope();

/* debugging */
void dump_symtab();


/* error reporting, functions that can be called from any component */
void error(const char *, ...);

#endif
