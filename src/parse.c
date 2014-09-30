#include "lcc.h"

#include <stdlib.h>
#include <string.h>

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
    eof = !get_token(&t);
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
    node->value = 0;
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
static node_t *constant_expression();
static node_t *postfix_expression();
static node_t *primary_expression();

static void output_tree(int indent, struct node *tree);


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
    if (!strcmp("integer", tree->text)) {
        printf(" %ld", tree->value);
    } else if (tree->token.value != NULL) {
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
                for (i = 0; i < type->d.func.n_args; ++i) {
                    if (type->d.func.params[i] == NULL) {
                        error("Missing parameter name at position %d, aborting", i + 1);
                        exit(1);
                    }
                    sym_add(type->d.func.params[i], type->d.func.args[i]);
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
    type->d.basic.qualifier = NONE_Q;
    type->d.basic.type = NONE_T;
    while (1) {
        switch (peek()) {
            case AUTO: case REGISTER: case STATIC: case EXTERN: case TYPEDEF:
                /* todo: something about storage class, maybe do it before this */
                break;
            case CHAR:
                type->d.basic.type = CHAR_T;
                break;
            case SHORT:
            case INT:
            case LONG:
            case SIGNED:
            case UNSIGNED:
                type->d.basic.type = INT64_T;
                break;
            case FLOAT:
            case DOUBLE:
                type->d.basic.type = DOUBLE_T;
                break;
            case VOID:
                type->d.basic.type = VOID_T;
                break;
            case CONST:
                type->d.ptr.qualifier |= CONST_Q;
                break;
            case VOLATILE:
                type->d.ptr.qualifier |= VOLATILE_Q;
                break;
            default:
                end = 1;
        }
        if (end) break;
        consume(peek());
    }
    if (type->d.basic.type == NONE_T) {
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
    type->d.ptr.to = base;
    base = type;
    consume('*');
    while (peek() == CONST || peek() == VOLATILE) {
        type->d.ptr.qualifier |= (readtoken().type == CONST) ? CONST_Q : VOLATILE_Q;
    }
    return type;
}

static typetree_t *
direct_declarator(typetree_t *base, const char **symbol)
{
    typetree_t *type = base;
    switch (peek()) {
        case IDENTIFIER: 
            *symbol = readtoken().value;
            break;
        case '(':
            consume('(');
            type = declarator(base, symbol);
            consume(')');
            break;
        default: break;
    }
    /* left-recursive declarations like 'int foo[10][5];' */
    while (peek() == '[' || peek() == '(') {
        switch (peek()) {
            case '[':
                type = init_typetree(ARRAY);
                type->d.arr.of = base;
                consume('[');
                if (peek() != ']') {
                    node_t *expr = constant_expression();
                    if (strcmp("integer", expr->text)) {
                        error("Array declaration must be a compile time constant, aborting");
                        exit(1);
                    }
                    if (expr->value < 1) {
                        error("Invalid array size %ld, aborting");
                        exit(1);
                    }
                    type->d.arr.size = expr->value;
                }
                consume(']');
                break;
            case '(': {
                consume('(');
                type = parameter_list(base);
                consume(')');
                break;
            }
            default: break;
        }
    }
    return type;
}

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list;
 * even though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static typetree_t *
parameter_list(typetree_t *base)
{
    typetree_t *type = init_typetree(FUNCTION), **args = NULL;
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
    
    type->d.func.ret = base;
    type->d.func.n_args = nargs;
    type->d.func.args = args;
    type->d.func.params = params;
    return type;
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

static node_t *assignment_expression();
static node_t *conditional_expression();
static node_t *logical_expression();
static node_t *or_expression();
static node_t *and_expression();
static node_t *equality_expression();
static node_t *relational_expression();
static node_t *shift_expression();
static node_t *additive_expression();
static node_t *multiplicative_expression();
static node_t *cast_expression();
static node_t *unary_expression();

static node_t *
expression()
{
    node_t *node = assignment_expression();
    return node;
}

static node_t *
assignment_expression()
{
    node_t *node = conditional_expression();
    return node;
}

static node_t *
constant_expression()
{
    node_t *node = conditional_expression();
    return node;
}

static node_t *
conditional_expression()
{
    node_t *node = logical_expression();
    if (peek() == '?') {
        node_t *parent = init_node("ternary-expression", 0);
        consume('?');
        addchild(parent, node);
        addchild(parent, expression());
        consume(':');
        addchild(parent, conditional_expression());
        node = parent;
    }
    return node;
}

/* merge AND/OR */
static node_t *
logical_expression()
{
    node_t *left = or_expression();
    while (peek() == LOGICAL_OR || peek() == LOGICAL_AND) {
        node_t *right, *parent;
        struct token t = readtoken();
        right = and_expression();
        if (!strcmp("integer", left->text) && !strcmp("integer", right->text)) {
            if (*t.value == LOGICAL_OR)
                left->value = left->value || right->value;
            else
                left->value = left->value && right->value;
        } else {
            parent = init_node("logical-expression", 2);
            parent->token = t;
            addchild(parent, left);
            addchild(parent, right);
            left = parent;
        }
    }
    return left;
}

/* merge OR/XOR */
static node_t *
or_expression()
{
    node_t *left = and_expression();
    while (peek() == '|' || peek() == '^') {
        node_t *right, *parent;
        struct token t = readtoken();
        right = and_expression();
        if (!strcmp("integer", left->text) && !strcmp("integer", right->text)) {
            if (*t.value == '|')
                left->value |= right->value;
            else
                left->value ^= right->value;
        } else {
            parent = init_node("or-expression", 2);
            parent->token = t;
            addchild(parent, left);
            addchild(parent, right);
            left = parent;
        }
    }
    return left;
}

static node_t *
and_expression()
{
    node_t *left = equality_expression();
    while (peek() == '&') {
        node_t *right, *parent;
        struct token t = readtoken();
        right = equality_expression();
        if (!strcmp("integer", left->text) && !strcmp("integer", right->text)) {
            left->value &= right->value;
        } else {
            parent = init_node("and-expression", 2);
            parent->token = t;
            addchild(parent, left);
            addchild(parent, right);
            left = parent;
        }
    }
    return left;
}

static node_t *
equality_expression()
{
    return relational_expression();
}

static node_t *
relational_expression()
{
    return shift_expression();
}

static node_t *
shift_expression()
{
    return additive_expression();
}

static node_t *
additive_expression()
{
    node_t *left = multiplicative_expression();
    while (peek() == '+' || peek() == '-') {
        node_t *right, *parent;
        struct token t = readtoken();
        right = multiplicative_expression();
        if (!strcmp("integer", left->text) && !strcmp("integer", right->text)) {
            if (*t.value == '+') {
                left->value += right->value;
            } else {
                left->value -= right->value;
            }
        } else {
            parent = init_node("additive-expression", 2);
            parent->token = t;
            addchild(parent, left);
            addchild(parent, right);
            left = parent;
        }
    }
    return left;
}

static node_t *
multiplicative_expression()
{
    node_t *left = cast_expression();
    while (peek() == '*' || peek() == '/' || peek() == '%') {
        node_t *right, *parent;
        struct token t = readtoken();
        right = cast_expression();
        if (!strcmp("integer", left->text) && !strcmp("integer", right->text)) {
            switch (*t.value) {
                case '*':
                    left->value *= right->value;
                    break;
                case '/':
                    left->value /= right->value;
                    break;
                default:
                    left->value %= right->value;
                    break;
            }
        } else {
            parent = init_node("multiplicative-expression", 2);
            parent->token = t;
            addchild(parent, left);
            addchild(parent, right);
            left = parent;
        }
    }
    return left;
}

static node_t *
cast_expression()
{
    return unary_expression();
}

static node_t *
unary_expression()
{
    return postfix_expression();
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
            node->value = atoi(node->token.value);
            break;
        default:
            error("Unexpected token '%s', not a valid primary expression", readtoken().value);
            exit(0);
    }
    return node;
}
