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
    const char *value;
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

void init_parsing(FILE *fd);
node_t *parse();


/* type analysis */

enum type { POINTER, T_INT, T_LONG, T_FLOAT, T_DOUBLE, T_CHAR, FUNCTION };
enum flag { T_CONST = 0x01, T_EXTERN = 0x02, T_STATIC = 0x04 };

typedef struct typestack {
    enum type type;
    enum flag flag;

    /* if pointer */
    struct typestack *ptrto;

    /* if function */
    struct typestack **args;
    unsigned n_args;
    struct typestack *retval;
} tstack_t;

/* static int *foo;
 * ( .name = "foo", .type )
 *                     |
 *          ( .type = POINTER, .ptrto )
 *                               |
 *                       ( .type = T_INT, flag = 0x02 )
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



/* IR */

void codegen();
void printir(FILE *);




/* error reporting, functions that can be called from any component */
void error(const char *, ...);

#endif
