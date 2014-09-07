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
static node_t *init_declarator_list();
static node_t *declarator();
static node_t *direct_declarator();
static node_t *identifier();
static node_t *pointer();
static node_t *type_qualifier_list();
static node_t *parameter_type_list();
static node_t *parameter_list();
static node_t *parameter_declaration();
static node_t *compound_statement();
static node_t *init_declarator();

/* External interface */
node_t *
parse(FILE *fd)
{
    input = fd;
    return translation_unit();
}

static node_t *
translation_unit()
{
    node_t *root = init_node("translation-unit", 1);
    root->children[0] = declaration();
    return root;
}


/* Declarations, statements that reserve a storage location. Virtually the
 * same as function definitions, so merge them into one and to postvalidation.
 *
 * function-definition -> declaration-specifiers declarator compound-statement
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
    node_t *declspec, *init_decl_list, *node;
    declspec = declaration_specifiers();
    init_decl_list = init_declarator_list();
    switch (peek()) {
        case ';':
            consume(';');
            node = init_node("declaration", 2);
            break;
        case '{': {
            // lift (init_declarator-list (init-declarator (declarator))
            node_t *init_decl = init_decl_list->children[0];
            node_t *declarator = init_decl->children[0];
            if  (init_decl_list->nc != 1 || init_decl->nc != 1) {
                fprintf(stderr, "Invalid function definition syntax");
                exit(0);
            }
            // todo: free init_decl_list and init_decl
            init_decl_list = declarator;
            node = init_node("function-definition", 3);
            node->children[2] = compound_statement();
            break;
        }
    }
    node->children[0] = declspec;
    node->children[1] = init_decl_list;
    return node;
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
    node_t *node = init_node("init-declarator-list", 2);
    node->children[0] = init_declarator();
    if (peek() == ',') {
        consume(',');
        node->children[1] = init_declarator_list();
    } else {
        node->nc = 1;
    }
    return node;
}

static node_t *
init_declarator()
{
    node_t *node = init_node("init_declarator", 1);
    node->children[0] = declarator();
    // todo: initialization
    return node;
}


/* declarator ->
 *      | [pointer] direct-declarator
 *
 * pointer -> 
 *      | '*' [type-qualifier-list]
 *      | '*' [type-qualifier-list] pointer
 *
 * direct-declarator ->
 *      | identifier
 *      | '(' declarator ')'
 *      | direct-declarator '[' [constant-expression] ']' // array declarator
 *      | direct-declarator '(' parameter-type-list ')'   // function declarator
 *      x direct-declarator '(' [identifier-list] ')'     // old-style function declaration
 *
 * parameter-type-list ->
 *      | parameter-list
 *      | parameter-list ',' '...'
 *
 * parameter-list ->
 *      | parameter-declaration
 *      | parameter-list ',' parameter-declaration
 *
 * parameter-declaration ->
 *      | declaration-specifiers declarator
 *      | declaration-specifiers [abstract-declarator] 
 *
 * abstract-declarator ->
 *      | pointer
 *      | [pointer] direct-abstract-declarator
 *
 * direct-abstract-declarator ->
 *      | '(' abstract-declarator ')'
 *      | direct-abstract-declarator '[' [constant-expression] ']' // array declarator
 *      | direct-abstract-declarator '(' parameter-type-list ')'   // function declarator
 */
static node_t *
declarator()
{
    node_t *node = init_node("declarator", 2);
    if (peek() == '*') {
        node->children[0] = pointer();
    } else {
        node->nc = 1;
    }
    node->children[node->nc - 1] = direct_declarator();
    return node;
}

static node_t *
pointer()
{
    node_t *qualifiers, *node = init_node("pointer", 2);
    node->token = readtoken();
    qualifiers = type_qualifier_list();
    if (qualifiers != NULL) {
        node->children[0] = qualifiers;
        if (peek() == '*') {
            node->children[1] = pointer();
        } else {
            node->nc = 1;
        }
    } else {
        if (peek() == '*') {
            node->children[0] = pointer();
            node->nc = 1;
        } else {
            node->nc = 0;
        }
    }
    return node;
}

static node_t *
type_qualifier_list()
{
    node_t *child, *node = init_node("type-qualifier-list", 2);
    switch (peek()) {
        case CONST:
        case VOLATILE:
            node->children[0] = init_node("type-qualifier", 0);
            node->children[0]->token = readtoken();
            node->children[1] = type_qualifier_list();
            break;
        default:
            node->nc = 1;
    }
    return node;
}

static node_t *
direct_declarator()
{
    node_t *node;
    switch (peek()) {
        case IDENTIFIER:
            node = init_node("direct-declarator", 2);
            node->children[0] = identifier();
            break;
        case '(':
            node = init_node("direct-declarator", 2);
            consume('(');
            node->children[0] = declarator();
            consume(')');
            break;
    }
    switch (peek()) {
        case '[':
            consume('[');
            //node->children[1] = contant_expression();
            // do something simple for now, just a number constant
            if (peek() != ']') {
                node->children[1] = init_node("constant-expression", 0);
                node->children[1]->token = readtoken();
            }
            consume(']');
            break;
        case '(':
            consume('(');
            node->children[1] = parameter_type_list();
            consume(')');
            break;
        default:
            node->nc = 1;
    }
    return node;
}

static node_t *
identifier()
{
    node_t *node = init_node("identifier", 0);
    node->token = readtoken();
    return node;
}

/*
 * parameter-type-list ->
 *      | parameter-list
 *      | parameter-list ',' '...'
 *
 * parameter-list ->
 *      | parameter-declaration
 *      | parameter-list ',' parameter-declaration
 *
 * parameter-declaration ->
 *      | declaration-specifiers declarator
 *      x declaration-specifiers [abstract-declarator] // assume abstract-declarator = declarator
 */
static node_t *
parameter_type_list()
{
    node_t *node = init_node("parameter-type-list", 1);
    node->children[0] = parameter_list();
    if (peek() == DOTS) {
        node->token = readtoken();
    }
    return node;
}

static node_t *
parameter_list()
{
    node_t *node = init_node("parameter-list", 2);
    node->children[0] = parameter_declaration();
    if (peek() == ',') {
        consume(',');
        if (peek() == DOTS) {
            node->nc = 1;
            return node;
        }
        node->children[1] = parameter_list();
    } else {
        node->nc = 1;
    }
    return node;
}

static node_t *
parameter_declaration()
{
    node_t *node = init_node("parameter-declaraton", 2);
    node->children[0] = declaration_specifiers();

    // No way to know if we recurse into declarator or abstract-declarator here.
    // FIRST(declarator) = FIRST(abstract-declarator) = { IDENTIFIER, '*', '(' }
    // Assume declarator = abstract-declarator, and do additional postprocessing
    // to validate if the parsing was correct.
    switch (peek()) {
        case IDENTIFIER:
        case '*':
        case '(':
            node->children[1] = declarator();
            break;
        default:
            node->nc = 1;
    }
    return node;
}

static node_t *
compound_statement()
{
    node_t *node = init_node("compound-statement", 0);
    consume('{');
    // todo: declaration list and statements
    consume('}');
    return node;
}
