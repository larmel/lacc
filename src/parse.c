#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>

static FILE *input;
static struct node *tree;

/* Tokenization interface and helper functions */
static struct token peek_value;
static int has_value;

static struct token
readtoken()
{
    if (has_value) {
        has_value = 0;
        return peek_value;
    }
    struct token t;
    int n = get_token(input, &t);
    return t;
}

static enum token_type
peek()
{
    if (!has_value) {
        peek_value = readtoken();
        has_value = 1;
    }
    return peek_value.type;
}

static void
consume(enum token_type expected)
{
    struct token t = readtoken();
    if (t.type != expected) {
        fprintf(stderr, "Unexpected token %s, aborting\n", (char *) t.value);
        exit(1);
    }
}


/* Parse tree helper functions */
static struct node *
init_node(char *name, size_t n)
{
    struct node *node = malloc(sizeof(struct node));
    node->text = name;
    node->nc = n;
    if (n) node->children = malloc(sizeof(struct node*) * n);
    /* todo: add to free list */
    return node;
}


static node_t *translation_unit();
static node_t *declaration();
static node_t *declaration_specifiers();
static node_t *storage_class_specifier();
static node_t *init_declarator_list();

/* External interface */
node_t *
parse(FILE *fd)
{
    input = fd;
    return translation_unit();
}


/* translation_unit -> function_definition | declaration
 */
static node_t *
translation_unit()
{
    node_t *root = init_node("translation-unit", 1);
    root->children[0] = declaration();
    return root;
}


/* Declarations, statements that reserve a storage location
 * Examples:
 * int;
 * > (decl (decl-spec (type-spec "int")) (identifier "a"))
 *
 * extern void a;
 * > (decl (decl-spec (storage-class-spec "extern") (decl-spec (type-spec "void"))) (identifier "a"))
 * 
 * 'a' is the declarator, which is a whole other beast. Start
 * with something simple, only caring about the storage stuff.
 *
 * declaration -> declaration_specifiers [init_declarator_list] ';'
 *
 * declaration_specifiers -> 
 *      storage_class_specifier [declaration_specifiers]
 *      type_specifier [declaration_specifiers]
 *      type_qualifier [declaration_specifiers]
 *
 * storage_class_specifier ->
 *      "auto" | "register" | "static" | "extern" | "typedef"
 *
 * type_specifier ->
 *      "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" | "unsigned"
 *
 * type_qualifier ->
 *      "const" | "volatile"
 *
 * init_declarator_list ->
 *      identifier ; simplification
 */
static node_t *
declaration()
{
    node_t *decl = init_node("declaration", 2);
    decl->children[0] = declaration_specifiers();
    decl->children[1] = init_declarator_list();
    consume(';');
    return decl;
}

static node_t *
declaration_specifiers()
{
    node_t *declspec, *node = NULL;
    switch (peek()) {
        case AUTO: case REGISTER: case STATIC: case EXTERN: case TYPEDEF:
            node = init_node("storage-class-specifier", 0);
            node->token = readtoken();
            break;
        case VOID: case CHAR: case SHORT: case INT: case LONG: case FLOAT:
        case DOUBLE: case SIGNED: case UNSIGNED:
            node = init_node("type-specifier", 0);
            node->token = readtoken();
            break;
        case CONST:
        case VOLATILE:
            node = init_node("type-qualifier", 0);
            node->token = readtoken();
            break;
    }

    if (node != NULL) {
        declspec = init_node("declaration-specifiers", 2);
        declspec->children[0] = node;
        declspec->children[1] = declaration_specifiers();
        return declspec;
    }

    /* need guard somewhere that the list is of at least length 1 */
    return NULL;
}

static node_t *
init_declarator_list()
{
    node_t *declarlist = init_node("init-declarator-list", 0);
    declarlist->token = readtoken();
    return declarlist;
}
