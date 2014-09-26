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
    STAR = '*'
} token_t;

struct token
{
    enum token_type type;
    const char *value;
};

int get_token(FILE *input, struct token* t);


/* parsing */

struct typetree;

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

enum tree_type { BASIC, POINTER, FUNCTION };
enum data_type { NONE_T, CHAR_T, INT64_T, DOUBLE_T, VOID_T };
enum qualifier { CONST_Q = 0x1, VOLATILE_Q = 0x2, NONE_Q = 0x0 };

typedef struct typetree {
    enum tree_type type;
    union {
        struct {
            enum data_type type;
            enum qualifier qualifier;
        } basic;
        struct {
            struct typetree *to;
            enum qualifier qualifier;
        } ptr;
        struct {
            struct typetree **args;
            const char **params;
            unsigned n_args;
            struct typetree *ret;
        } func;
    } data;
} typetree_t;

enum storageclass { STORAGE_EXTERN, STORAGE_STATIC };

/* static int *foo, *bar;
 * ( .name = "foo", .type )    ( .name = "bar", .type )
 *                     |                         |
 *          ( .type = POINTER, .ptrto )
 *                               |
 *                       ( .type = T_INT, flag = 0x02 )
 */
typedef struct symbol {
    const char *name;
    int depth;
    struct typetree *type;
    enum storageclass storage;
} symbol_t;

/* resolve symbol in current scope, or NULL if not found */
symbol_t *sym_lookup(const char *);

/* add symbol to current scope */
symbol_t *sym_add(const char *, typetree_t *);

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
