#ifndef ARRAY_H
#define ARRAY_H

#define ARRAY_CAPACITY_INITIAL 16
#define ARRAY_CAPACITY_GROWTH(cap) (cap * 2)

/* Declare a struct or union member as array of given type. Intended
 * usage:
 *
 * struct code {
 *     array_of(struct op) operations;
 * };
 *
 */
#define array_of(T) \
    struct {                                                                   \
        unsigned capacity;                                                     \
        unsigned length;                                                       \
        T *data;                                                               \
    }

/* Add element to array. Expands to a block statement.
 */
#define array_push_back(arr, elem) \
    do {                                                                       \
        if ((arr)->length == (arr)->capacity) {                                \
            if (!(arr)->capacity) {                                            \
                (arr)->capacity = ARRAY_CAPACITY_INITIAL;                      \
                (arr)->data = calloc((arr)->capacity, sizeof(elem));           \
            } else {                                                           \
                (arr)->capacity = ARRAY_CAPACITY_GROWTH((arr)->capacity);      \
                (arr)->data =                                                  \
                    realloc((arr)->data, (arr)->capacity * sizeof(elem));      \
            }                                                                  \
        }                                                                      \
        (arr)->data[(arr)->length++] = elem;                                   \
    } while (0)

/* Copy elements from b to end of a, keeping b unchanged. Expands to a
 * block statement.
 */
#define array_concat(a, b) \
    do {                                                                       \
        if ((a)->capacity < array_len(a) + array_len(b)) {                     \
            (a)->capacity =                                                    \
                array_len(a) + array_len(b) + ARRAY_CAPACITY_INITIAL;          \
            (a)->data = realloc((a)->data, (a)->capacity * sizeof(*(a)->data));\
        }                                                                      \
        memcpy(                                                                \
            (a)->data + array_len(a),                                          \
            (b)->data,                                                         \
            array_len(b) * sizeof(*(a)->data));                                \
        (a)->length += array_len(b);                                           \
    } while (0)

/* Remove and return last element in array. Expands to expression of
 * element type.
 */
#define array_pop_back(arr) \
    (arr)->data[--(arr)->length]

/* Get element from array. Expands to expression, of type being
 * whatever is stored in the array.
 */
#define array_get(arr, i) \
    (arr)->data[i]

/* Return length of the array. Expands to expression of unsigned integer
 * type.
 */
#define array_len(arr) \
    (arr)->length

/* Free allocated memory, making the array empty. Expands to a block
 * statement.
 */
#define array_clear(arr) \
    do {                                                                       \
        if ((arr)->capacity)                                                   \
            free((arr)->data);                                                 \
        (arr)->length = 0;                                                     \
        (arr)->capacity = 0;                                                   \
        (arr)->data = NULL;                                                    \
    } while (0)

#endif
