#include "symbol.h"
#include "error.h"

#include <stdio.h>
#include <stdlib.h>

typetree_t *
type_init(enum tree_type type)
{
    typetree_t *tree = calloc(1, sizeof(typetree_t));
    tree->type = type;
    switch (type) {
        case CHAR_T:
            tree->size = 1;
            break;
        default:
            tree->size = 8;
    }
    return tree;
}

int
type_equal(const typetree_t *a, const typetree_t *b)
{
    if (a == NULL && b == NULL) return 1;
    if (a == NULL || b == NULL) return 0;
    if (a->type == b->type) return 1;
    /* todo */
    return 0;
}

/* Resulting type of a <op> b */
const typetree_t *
type_combine(const typetree_t *a, const typetree_t *b)
{
    char *stra, *strb;
    if (!a || !b) {
        error("Internal error: cannot combine NULL type.");
        exit(1);
    }
    if (type_equal(a, b))
        return a;

    /* Arrays decay into pointer */
    if (a->type == ARRAY) {
        typetree_t *ptr = type_init(POINTER);
        ptr->next = a->next;
        a = ptr;
    }

    if (a->type == POINTER && b->type == INT64_T)
        return a;

    stra = typetostr(a);
    strb = typetostr(b);
    error("Cannot combine types `%s` and `%s`.", stra, strb);
    free(stra);
    free(strb);
    exit(1);
    return NULL;
}

const typetree_t *
type_deref(const typetree_t *t)
{
    if (t->type != POINTER && t->type == ARRAY) {
        char *str = typetostr(t);
        error("Cannot dereference non-pointer type `%s`.", str);
        free(str);
        return NULL;
    }
    return t->next;
}

unsigned
type_size(const typetree_t *t)
{
    unsigned size = t->size;
    if (t->length)
        size *= t->length;
    return size;
}

/* Print type to buffer, returning how many characters were written. */
static int
snprinttype(const typetree_t *tree, char *s, int size)
{
    int i, w = 0;
    if (!tree)
        return w;
    switch (tree->type) {
        case CHAR_T:
            w = snprintf(s, size, "char");
            break;
        case INT64_T:
            w = snprintf(s, size, "int");
            break;
        case DOUBLE_T:
            w = snprintf(s, size, "double");
            break;
        case VOID_T:
            w = snprintf(s, size, "void");
            break;
        case POINTER:
            if (tree->flags) {
                if (tree->flags & CONST_Q) 
                    w += snprintf(s, size, "const ");
                if (tree->flags & VOLATILE_Q) 
                    w += snprintf(s + w, size - w, "volatile ");
            }
            w += snprintf(s + w, size - w, "* ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        case FUNCTION:
            w += snprintf(s, size, "(");
            for (i = 0; i < tree->n_args; ++i) {
                w += snprinttype(tree->args[i], s + w, size - w);
                if (i < tree->n_args - 1)
                    w += snprintf(s + w, size - w, ", ");
            }
            w += snprintf(s + w, size - w, ") -> ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        case ARRAY:
            if (tree->length > 0)
                w += snprintf(s, size, "[%u] ", tree->length);
            else
                w += snprintf(s, size, "[] ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        default:
            break;
    }
    return w;
}

/* For debug printing and error reporting types. Caller should free memory. */
char *
typetostr(const typetree_t *type)
{
    char *text = malloc(512 * sizeof(char));
    snprinttype(type, text, 511);
    return text;
}
