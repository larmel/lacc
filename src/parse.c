#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>

static FILE *input;

/* Tokenization interface and helper functions */
static struct token peek_value;
static int has_value;
static int eof;

static struct token
readtoken()
{
    struct token t;
    if (has_value) {
        has_value = 0;
        return peek_value;
    }
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

static void
addchild(node_t *node, node_t *child)
{
    if (node->nc == node->cap) {
        node->cap += 8;
        node->children = realloc(node->children, sizeof(node_t *) * node->cap);
    }
    node->children[node->nc] = child;
    node->nc++;
}

static typetree_t *
init_typetree(enum tree_type type)
{
    typetree_t *tree = malloc(sizeof(typetree_t));
    tree->type = type;
    return tree;
}

static node_t *declaration();
static typetree_t *declaration_specifiers();
static typetree_t *declarator(typetree_t *, const char **);
static typetree_t *pointer(typetree_t *);
static typetree_t *direct_declarator(typetree_t *, const char **);
static typetree_t *parameter_list(typetree_t *);
static node_t *block();
static node_t *statement();
static node_t *identifier();
static node_t *expression();
static node_t *postfix_expression();
static node_t *primary_expression();

static void output_tree(int indent, struct node *tree);

void
init_parsing(FILE *fd)
{
    input = fd;
}

/* External interface */
node_t *
parse()
{
    peek();
    while (!eof) {
        node_t *node = declaration();
        if (node != NULL) {
            output_tree(0, node);
            puts("");
            return node;
        }
        peek();
    }
    return NULL;
}

/* Print parse tree in human readable format */
static void 
output_tree(int indent, struct node *tree)
{
    int i;
    if (tree == NULL) {
        printf("%*s(null)", indent, "");
        return;
    }
    printf("%*s(%s", indent, "", tree->text);
    if (tree->token.value != NULL) {
        printf(" \"%s\"", tree->token.value);
    }
    if (tree->nc > 0) {
        printf("\n");
        for (i = 0; i < tree->nc; ++i) {
            output_tree(indent + 2, tree->children[i]);
            if (i < tree->nc - 1)
                printf("\n");
        }
    }
    printf(")");
}

/* Return either an initialized declaration, or a function definition.
 * Forward declarations are just registered in the symbol table. 
 */
static node_t *
declaration()
{
    node_t *node = NULL, *child = NULL;
    symbol_t *symbol;
    typetree_t *type, *base = declaration_specifiers();
    int i;

    do {
        const char *name = NULL;
        type = declarator(base, &name);
        symbol = sym_add(name, type);
        switch (peek()) {
            case ';':
                consume(';');
                return node;
            case '=':
                consume('=');
                if (node == NULL)
                    node = init_node("declaration", 0);
                child = init_node("assignment", 0);
                addchild(node, child);
                addchild(child, primary_expression()); /* todo: assignment-expression */
                child->token.type = IDENTIFIER;
                child->token.value = name;
                if (peek() != ',') {
                    consume(';');
                    return node;
                }
                break;
            case '{':
                if (type->type != FUNCTION || node != NULL || symbol->depth > 0) {
                    error("Invalid function definition, aborting");
                    exit(1);
                }
                node = init_node("function-definition", 0);
                node->token.type = IDENTIFIER;
                node->token.value = name;
                push_scope();
                for (i = 0; i < type->data.func.n_args; ++i) {
                    if (type->data.func.params[i] == NULL) {
                        error("Missing parameter name at position %d, aborting", i + 1);
                        exit(1);
                    }
                    sym_add(type->data.func.params[i], type->data.func.args[i]);
                }
                addchild(node, block());
                pop_scope();
                return node;
            default: 
                break;
        }
        consume(',');
    } while (1);
}

static typetree_t *
declaration_specifiers()
{
    int end = 0;
    typetree_t *type = init_typetree(BASIC);
    type->data.basic.qualifier = NONE_Q;
    type->data.basic.type = NONE_T;
    while (1) {
        switch (peek()) {
            case AUTO: case REGISTER: case STATIC: case EXTERN: case TYPEDEF:
                /* todo: something about storage class, maybe do it before this */
                break;
            case CHAR:
                type->data.basic.type = CHAR_T;
                break;
            case SHORT:
            case INT:
            case LONG:
            case SIGNED:
            case UNSIGNED:
                type->data.basic.type = INT64_T;
                break;
            case FLOAT:
            case DOUBLE:
                type->data.basic.type = DOUBLE_T;
                break;
            case VOID:
                type->data.basic.type = VOID_T;
                break;
            case CONST:
                type->data.ptr.qualifier |= CONST_Q;
                break;
            case VOLATILE:
                type->data.ptr.qualifier |= VOLATILE_Q;
                break;
            default:
                end = 1;
        }
        if (end) break;
        consume(peek());
    }
    if (type->data.basic.type == NONE_T) {
        error("Missing type specifier, aborting");
        exit(1);
    }
    return type;
}

static typetree_t *
declarator(typetree_t *base, const char **symbol)
{
    while (peek() == '*') {
        base = pointer(base);
    }
    return direct_declarator(base, symbol);
}

static typetree_t *
pointer(typetree_t *base)
{
    typetree_t *type = init_typetree(POINTER);
    type->data.ptr.to = base;
    base = type;
    consume('*');
    while (peek() == CONST || peek() == VOLATILE) {
        type->data.ptr.qualifier |= (readtoken().type == CONST) ? CONST_Q : VOLATILE_Q;
    }
    return type;
}

static typetree_t *
direct_declarator(typetree_t *base, const char **symbol)
{
    switch (peek()) {
        case IDENTIFIER: 
            *symbol = readtoken().value;
            break;
        case '(':
            consume('(');
            base = declarator(base, symbol);
            consume(')');
            break;
        default: break;
    }
    /* left-recursive declarations like 'int foo[10][5];' */
    while (peek() == '[' || peek() == '(') {
        switch (peek()) {
            case '[':
                consume('[');
                if (peek() != ']') {
                    /* constant expression, evaluate immediately (no parse tree emitted) */
                    readtoken();
                    /* todo: add array type */
                }
                consume(']');
                break;
            case '(': {
                consume('(');
                base = parameter_list(base);
                consume(')');
                break;
            }
            default: break;
        }
    }
    return base;
}

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list;
 * even though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static typetree_t *
parameter_list(typetree_t *base)
{
    typetree_t *func = init_typetree(FUNCTION), **args = NULL;
    const char **params = NULL;
    int nargs = 0;

    while (peek() != ')') {
        const char *symbol = NULL;
        typetree_t *decl = declaration_specifiers();
        decl = declarator(decl, &symbol);

        nargs++;
        args = realloc(args, sizeof(typetree_t *) * nargs);
        params = realloc(params, sizeof(char *) * nargs);
        args[nargs - 1] = decl;
        params[nargs - 1] = symbol;

        if (peek() != ',') break;
        consume(',');
        if (peek() == ')') {
            error("Trailing comma in parameter list, aborting");
            exit(1);
        }
        if (peek() == DOTS) {
            consume(DOTS); /* todo: add vararg type */
            break;
        }
    }
    
    func->data.func.ret = base;
    func->data.func.n_args = nargs;
    func->data.func.args = args;
    func->data.func.params = params;
    return func;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
static node_t *
block()
{
    node_t *node = init_node("block", 32), *child;
    consume('{');
    while (peek() != '}') {
        if (peek() == ';') {
            consume(';');
            continue;
        }
        child = statement();
        if (child != NULL)
            addchild(node, child);
    }
    consume('}');
    return node;
}

static node_t *
statement()
{
    node_t *node;
    enum token_type t = peek();
    switch (t) {
        case '{':
            push_scope();
            node = block();
            pop_scope();
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
identifier()
{
    node_t *node = init_node("identifier", 0);
    struct token name = readtoken();
    symbol_t *symbol = sym_lookup(name.value);
    if (symbol == NULL) {
        error("Undefined symbol '%s', aborting", name.value);
        exit(0);
    }
    node->token = name;
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
                /* addchild(parent, argument_expression_list()); */
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
