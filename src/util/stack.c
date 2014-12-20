#include "stack.h"

void
stack_push(stack_t *stack, void *object)
{
    if (stack->size == stack->capacity) {
        stack->capacity += 16;
        stack->values = realloc(stack->values, stack->capacity * sizeof(void*));
    }
    stack->values[stack->size] = object;
    stack->size++;
}

void *
stack_pop(stack_t *stack)
{
    if (stack->size > 0) {
        return stack->values[--stack->size];
    }
    return NULL;
}

void *
stack_peek(stack_t *stack)
{
    if (stack->size > 0) {
        return stack->values[stack->size - 1];
    }
    return NULL;
}
