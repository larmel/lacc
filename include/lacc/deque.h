#ifndef DEQUE_H
#define DEQUE_H

#include "array.h"

/*
 * Declare a type resembling a deque in C++, which can be efficiently
 * pushed and popped from either side.
 */
#define deque_of(T) \
    struct {                                                                   \
        unsigned cursor;                                                       \
        array_of(T) array;                                                     \
    }

#define deque_len(deq) \
    ((deq)->array.length - (deq)->cursor)

#define deque_push_back(deq, elem) \
    do {                                                                       \
        if ((deq)->cursor && (deq)->array.length == (deq)->array.capacity) {   \
            memmove(                                                           \
                (deq)->array.data,                                             \
                (deq)->array.data + (deq)->cursor,                             \
                deque_len(deq) * sizeof(elem));                                \
            (deq)->array.length = deque_len(deq);                              \
            (deq)->cursor = 0;                                                 \
        }                                                                      \
        array_push_back(&(deq)->array, elem);                                  \
    } while (0)

#define deque_back(deq) \
    deque_get(deq, deque_len(deq) - 1)

#define deque_pop_back(deq) \
    array_pop_back(&(deq)->array)

#define deque_pop_front(deq) \
    array_get(&(deq)->array, (deq)->cursor++)

#define deque_get(deq, i) \
    array_get(&(deq)->array, (deq)->cursor + i)

#define deque_empty(deq) \
    do {                                                                       \
        (deq)->cursor = 0;                                                     \
        array_empty(&(deq)->array);                                            \
    } while (0)

#define deque_destroy(deq) \
    do {                                                                       \
        (deq)->cursor = 0;                                                     \
        array_clear(&(deq)->array);                                            \
    } while (0)

#endif
