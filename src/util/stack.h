#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

/* Store and access pointers to arbitrary objects in a stack. */
typedef struct stack {
    void **values;

    size_t size;
    size_t capacity;
} stack_t;

void
stack_finalize(stack_t *stack);

void
stack_push(stack_t *stack, void *object);

void *
stack_pop(stack_t *stack);

void *
stack_peek(stack_t *stack);


#endif
