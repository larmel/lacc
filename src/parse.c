#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>

static FILE *input;
static struct node *tree;

/* Tokenization interface and helper functions */
static struct token peek_value;
static int has_value;
static int eof;

static struct token
readtoken()
{
    if (has_value) {
        has_value = 0;
        return peek_value;
    }
    struct token t;
    eof = (get_token(input, &t) == 0);
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
        error("Unexpected token %s, aborting\n", t.value);
        exit(1);
    }
}


/* Parse tree helper functions */
static struct node *
init_node(const char *name, size_t n)
{
    struct node *node = malloc(sizeof(node_t));
    node->text = name;
    node->nc = 0;
    node->cap = n;
    if (n) node->children = malloc(sizeof(node_t *) * node->cap);
    else node->children = NULL;
    /* todo: add to free list */
    return node;
}

void
addchild(node_t *node, node_t *child)
{
    if (node->nc == node->cap) {
        node->cap += 8;
        node->children = realloc(node->children, sizeof(node_t *) * node->cap);
    }
    node->children[node->nc] = child;
    node->nc++;
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
static node_t *parameter_list();
static node_t *parameter_declaration();
static node_t *block();
static node_t *init_declarator();
static node_t *statement();
static node_t *expression();
static node_t *postfix_expression();
static node_t *primary_expression();


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
    node_t *root = init_node("translation-unit", 16);
    push_scope();
    while (1) {
        peek();
        if (eof) break;
        addchild(root, declaration());
    }
    pop_scope();
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
    node_t *specifiers, *declaratorlist, *node;
    specifiers = declaration_specifiers();
    declaratorlist = init_declarator_list();
    switch (peek()) {
        case ';':
            consume(';');
            node = init_node("declaration", 2);
            addchild(node, specifiers);
            addchild(node, declaratorlist);
            break;
        case '{':
            if  (declaratorlist->nc != 1 || declaratorlist->children[0]->nc != 1) {
                error("Invalid function definition syntax");
                exit(0);
            }
            // lift (init_declarator-list (init-declarator (declarator))
            // todo: free discarded nodes
            node = init_node("function-definition", 3);
            addchild(node, specifiers);
            addchild(node, declaratorlist->children[0]->children[0]);
            addchild(node, block());
            break;
        default:
            error("Unexpected token '%s' trailing declaration, aborting", readtoken().value);
            exit(0);
    }
    return node;
}

static node_t *
declaration_specifiers()
{
    node_t *declspec, *node = init_node("declaration-specifiers", 8);
    do {
        switch (peek()) {
            case AUTO: case REGISTER: case STATIC: case EXTERN: case TYPEDEF:
                declspec = init_node("storage-class-specifier", 0);
                break;
            case VOID: case CHAR: case SHORT: case INT: case LONG: case FLOAT:
            case DOUBLE: case SIGNED: case UNSIGNED:
                declspec = init_node("type-specifier", 0);
                break;
            case CONST:
            case VOLATILE:
                declspec = init_node("type-qualifier", 0);
                break;
            default:
                /* no guarantee that the list is of at least length 1 */
                return node;
        }
        declspec->token = readtoken();
        addchild(node, declspec);
    } while (1);
}

static node_t *
init_declarator_list()
{
    node_t *node = init_node("init-declarator-list", 2);
    addchild(node, init_declarator());
    while (peek() == ',') {
        consume(',');
        addchild(node, init_declarator());
    }
    return node;
}

static node_t *
init_declarator()
{
    node_t *node = init_node("init-declarator", 1);
    addchild(node, declarator());
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
        addchild(node, pointer());
    }
    addchild(node, direct_declarator());
    return node;
}

static node_t *
pointer()
{
    node_t *qualifiers, *node = init_node("pointer", 2);
    node->token = readtoken();
    if (peek() == CONST || peek() == VOLATILE) {
        addchild(node, type_qualifier_list());
    }
    if (peek() == '*') {
        addchild(node, pointer());
    }
    return node;
}

static node_t *
type_qualifier_list()
{
    node_t *child, *node = init_node("type-qualifier-list", 4);
    while (peek() == CONST || peek() == VOLATILE) {
        child = init_node("type-qualifier", 0);
        child->token = readtoken();
        addchild(node, child);
    }
    return node;
}

static node_t *
direct_declarator()
{
    node_t *node = init_node("direct-declarator", 1);
    switch (peek()) {
        case IDENTIFIER:
            addchild(node, identifier());
            symbol_t *symbol = sym_add(node->children[0]->token.value);
            break;
        case '(':
            consume('(');
            addchild(node, declarator());
            consume(')');
            break;
    }
    // Handle left-recursiveness bottom up, declarations like 'int foo[10][5];'
    while (peek() == '[' || peek() == '(') {
        node_t *parent = init_node("direct_declarator", 2);
        addchild(parent, node);
        if (peek() == '[') {
            consume('[');
            if (peek() != ']') {
                // do something simple for now, just a number constant
                node_t *expr = init_node("constant-expression", 0);
                expr->token = readtoken();
                addchild(parent, expr);
            }
            consume(']');
        } else {
            consume('(');
            addchild(parent, parameter_list());
            consume(')');
        }
        node = parent;
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

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list;
 * even though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static node_t *
parameter_list()
{
    node_t *node = init_node("parameter-list", 8);
    if (peek() == DOTS) {
        error("Parameter list with varargs must contain at least one declaration");
        exit(0);
    }
    while (peek() != ')' && peek() != DOTS) {
        addchild(node, parameter_declaration());
        if (peek() == ',') {
            consume(',');
        } else break;
    }
    if (peek() == DOTS) {
        node->token = readtoken();
    }
    return node;
}

static node_t *
parameter_declaration()
{
    node_t *node = init_node("parameter-declaraton", 2);
    addchild(node, declaration_specifiers());

    // No way to know if we recurse into declarator or abstract-declarator here.
    // FIRST(declarator) = FIRST(abstract-declarator) = { IDENTIFIER, '*', '(' }
    // Assume declarator = abstract-declarator, and do additional postprocessing
    // to validate if the parsing was correct.
    switch (peek()) {
        case IDENTIFIER:
        case '*':
        case '(':
            addchild(node, declarator());
            break;
    }
    return node;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
static node_t *
block()
{
    node_t *node = init_node("block", 32);
    push_scope();
    consume('{');
    while (peek() != '}') {
        if (peek() == ';') {
            consume(';');
            continue;
        }
        addchild(node, statement());
    }
    consume('}');
    pop_scope();
    return node;
}

static node_t *
statement()
{
    node_t *node;
    enum token_type t = peek();
    switch (t) {
        case '{':
            node = block();
            break;
        case IF:
        case SWITCH:
            node = init_node("selection-statement", 3);
            node->token = readtoken();
            consume('(');
            addchild(node, expression());
            consume(')');
            addchild(node, statement());
            if (peek() == ELSE) {
                consume(ELSE);
                addchild(node, statement());
            }
            break;
        case WHILE:
        case DO:
        case FOR:
            node = init_node("iteration-statement", 4);
            node->token = readtoken();
            if (t == WHILE) {
                consume('(');
                addchild(node, expression());
                consume(')');
                addchild(node, statement());
            } else if (t == DO) {
                addchild(node, statement());
                consume(WHILE);
                consume('(');
                addchild(node, expression());
                consume(')');
            } else {
                consume('(');
                addchild(node, (peek() != ';') ? expression() : NULL);
                consume(';');
                addchild(node, (peek() != ';') ? expression() : NULL);
                consume(';');
                addchild(node, (peek() != ')') ? expression() : NULL);
                consume(')');
                addchild(node, statement());
            }
            break;
        case GOTO:
        case CONTINUE:
        case BREAK:
        case RETURN:
            node = init_node("jump-statement", 1);
            node->token = readtoken();
            if (t == GOTO && peek() == IDENTIFIER) {
                addchild(node, identifier());
            } else if (t == RETURN && peek() != ';') {
                addchild(node, expression());
            }
            consume(';');
            break;
        case CASE:
        case DEFAULT:
            node = init_node("labeled-statement", 2);
            node->token = readtoken();
            if (peek() == ':') {
                consume(':');
                addchild(node, statement());
            } else {
                addchild(node, primary_expression()); /* todo: constant_expression */
                consume(':');
                addchild(node, statement());
            }
            break;
        case IDENTIFIER: /* also part of label statement, need 2 lookahead */
        case INTEGER: /* todo: any constant value */
        case STRING:
        case '(':
            node = expression();
            consume(';');
            break;
        default:
            node = declaration();
    }
    return node;
}

static node_t *
expression()
{
    node_t *node = init_node("expression", 0);
    addchild(node, postfix_expression());
    return node;
}

/* This rule is left recursive, build tree bottom up
 */
static node_t *
postfix_expression()
{
    node_t *root = primary_expression();
    while (peek() == '[' || peek() == '(' || peek() == '.') {
        node_t *parent = init_node("postfix-expression", 2);
        addchild(parent, root);
        switch (peek()) {
            case '[':
                consume('[');
                addchild(parent, expression());
                consume(']');
                break;
            case '(':
                // addchild(parent, argument_expression_list());
                consume('(');
                consume(')');
                break;
            case '.':
                parent->token = readtoken();
                addchild(parent, identifier());
                break;
            default:
                error("Unexpected token '%s', not a valid postfix expression", readtoken().value);
                exit(0);
        }
        root = parent;
    }
    return root;
}

static node_t *
primary_expression()
{
    node_t *node;
    switch (peek()) {
        case IDENTIFIER:
            node = identifier();
            symbol_t *symbol = sym_lookup(node->token.value);
            if (symbol == NULL) {
                error("Undefined symbol '%s', aborting", node->token.value);
                exit(0);
            }
            break;
        case INTEGER:
            node = init_node("integer", 0);
            node->token = readtoken();
            break;
        default:
            error("Unexpected token '%s', not a valid primary expression", readtoken().value);
            exit(0);
    }
    return node;
}
