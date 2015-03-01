#include "error.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


typetree_t *
type_init(enum tree_type type)
{
    typetree_t *tree = calloc(1, sizeof(typetree_t));
    tree->type = type;
    switch (type) {
        case INTEGER:
        case REAL:
            tree->size = 4; /* default is 32 bit int and float */
            break;
        case FUNCTION:
        case ARRAY:
        case POINTER:
        case OBJECT:
        case NONE:
            tree->size = 8;
    }
    return tree;
}

int
type_equal(const typetree_t *a, const typetree_t *b)
{
    if (!a && !b) return 1;
    if (!a || !b) return 0;

    if (a->type == b->type
        && a->size == b->size
        && a->n_args == b->n_args
        && type_equal(a->next, b->next))
    {
        int i;

        for (i = 0; i < a->n_args; ++i) {
            if (!type_equal(a->args[i], b->args[i]))
                return 0;
        }
        return 1;   
    }
    return 0;
}

/* Resulting type of a <op> b */
const typetree_t *
type_combine(const typetree_t *a, const typetree_t *b)
{
    if (!a || !b) {
        error("Cannot combine NULL type.");
        exit(1);
    }

    /* Arrays decay into pointer */
    if (a->type == ARRAY) {
        typetree_t *ptr = type_init(POINTER);
        ptr->next = a->next;
        a = ptr;
    }
    if (b->type == ARRAY) {
        typetree_t *ptr = type_init(POINTER);
        ptr->next = b->next;
        b = ptr;
    }

    /* Pointer arithmetic */
    if (a->type == POINTER && b->type == INTEGER)
        return a;
    if (b->type == POINTER && a->type == INTEGER)
        return b;

    /* Integer promotion */
    if (a->type == INTEGER && b->type == INTEGER) {
        if (a->size > b->size) b = a;
        if (a->size < b->size) a = b;
    }

    if (!type_equal(a, b)) {
        char *stra, *strb;
        stra = typetostr(a);
        strb = typetostr(b);
        error("Cannot combine types `%s` and `%s`.", stra, strb);
        free(stra);
        free(strb);
        exit(1);
    }

    return a;
}

const typetree_t *
type_deref(const typetree_t *t)
{
    if (t->type != POINTER && t->type != ARRAY) {
        char *str = typetostr(t);
        error("Cannot dereference non-pointer type `%s`.", str);
        free(str);
        return NULL;
    }
    return t->next;
}

/* Validate that type p can be completed by applying size from q, and return
 * q as the result.
 */
const typetree_t *
type_complete(const typetree_t *p, const typetree_t *q)
{
    assert(!p->size && q->size);
    
    if (p->type != q->type || !type_equal(p->next, q->next)) {
        error("Incompatible specification of incomplete type.");
        exit(1);
    }

    return q;
}

/* Print type to buffer, returning how many characters were written.
 */
static int
snprinttype(const typetree_t *tree, char *s, int size)
{
    int i, w = 0;
    if (!tree)
        return w;

    if (tree->flags.funsigned)
        w += snprintf(s + w, size - w, "unsigned ");
    if (tree->flags.fconst)
        w += snprintf(s + w, size - w, "const ");
    if (tree->flags.fvolatile)
        w += snprintf(s + w, size - w, "volatile ");

    switch (tree->type) {
        case INTEGER:
            switch (tree->size) {
                case 1:
                    w += snprintf(s + w, size - w, "char");
                    break;
                case 2:
                    w += snprintf(s + w, size - w, "short");
                    break;
                case 4:
                    w += snprintf(s + w, size - w, "int");
                    break;
                case 8:
                    w += snprintf(s + w, size - w, "long");
                    break;
                default:
                    w += snprintf(s + w, size - w, "__int%d", tree->size * 8);
                    break;
            }
            break;
        case REAL:
            switch (tree->size) {
                case 4:
                    w += snprintf(s + w, size - w, "float");
                    break;
                case 8:
                    w += snprintf(s + w, size - w, "double");
                    break;
                default:
                    w += snprintf(s + w, size - w, "__real%d", tree->size * 8);
                    break;
            }
            break;
        case NONE:
            w += snprintf(s + w, size - w, "void");
            break;
        case POINTER:
            w += snprintf(s + w, size - w, "* ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        case FUNCTION:
            w += snprintf(s + w, size - w, "(");
            for (i = 0; i < tree->n_args; ++i) {
                w += snprinttype(tree->args[i], s + w, size - w);
                if (i < tree->n_args - 1)
                    w += snprintf(s + w, size - w, ", ");
            }
            w += snprintf(s + w, size - w, ") -> ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        case ARRAY:
            if (tree->size > 0)
                w += snprintf(s + w, size - w, "[%u] ", tree->size / tree->next->size);
            else
                w += snprintf(s + w, size - w, "[] ");
            w += snprinttype(tree->next, s + w, size - w);
            break;
        case OBJECT:
            w += snprintf(s + w, size - w, "{");
            for (i = 0; i < tree->n_args; ++i) {
                w += snprintf(s + w, size - w, ".%s::", tree->params[i]);
                w += snprinttype(tree->args[i], s + w, size - w);
                if (i < tree->n_args - 1)
                    s += snprintf(s + w, size - w, ", ");
            }
            w += snprintf(s + w, size - w, "}");
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
