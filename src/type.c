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

    error("Cannot combine types, aborting");
    print_type(a);
    puts("");
    print_type(b);
    puts("");
    exit(0);
    return NULL;
}

const typetree_t *
type_deref(const typetree_t *t)
{
    if (t->type == POINTER || t->type == ARRAY) {
        return t->next;
    }
    error("Cannot dereference non-pointer type, aborting");
    exit(0);
    return NULL;
}

void
print_type(const typetree_t *tree)
{
    int i;
    if (tree == NULL) return;
    switch (tree->type) {
        case CHAR_T:
            printf("char");
            break;
        case INT64_T:
            printf("int");
            break;
        case DOUBLE_T:
            printf("double");
            break;
        case VOID_T:
            printf("void");
            break;
        case POINTER:
            if (tree->flags) {
                if (tree->flags & CONST_Q) printf("const ");
                if (tree->flags & VOLATILE_Q) printf("volatile ");
            }
            printf("* ");
            print_type(tree->next);
            break;
        case FUNCTION:
            printf("(");
            for (i = 0; i < tree->n_args; ++i) {
                print_type(tree->args[i]);
                if (i < tree->n_args - 1)
                    printf(", ");
            }
            printf(") -> ");
            print_type(tree->next);
            break;
        case ARRAY:
            if (tree->length > 0)
                printf("[%u] ", tree->length);
            else 
                printf("[] ");
            print_type(tree->next);
            break;
        default: break;
    }
}
