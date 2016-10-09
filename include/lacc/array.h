#ifndef ARRAY_H
#define ARRAY_H

#define ARRAY_CAPACITY_INITIAL 16
#define ARRAY_CAPACITY_GROWTH(cap) (cap * 2)

/* Declare an object representing an array of given type. */
#define array_of(T) \
    struct {                                                                   \
        unsigned capacity;                                                     \
        unsigned length;                                                       \
        T *data;                                                               \
    }

#define array_increase_cap(arr) \
    do {                                                                       \
        if ((arr)->length == (arr)->capacity) {                                \
            if (!(arr)->capacity) {                                            \
                (arr)->capacity = ARRAY_CAPACITY_INITIAL;                      \
                (arr)->data = calloc((arr)->capacity, sizeof(*(arr)->data));   \
            } else {                                                           \
                (arr)->capacity = ARRAY_CAPACITY_GROWTH((arr)->capacity);      \
                (arr)->data =                                                  \
                    realloc(                                                   \
                        (arr)->data,                                           \
                        (arr)->capacity * sizeof(*(arr)->data));               \
                memset(                                                        \
                    (arr)->data + (arr)->length,                               \
                    0,                                                         \
                    sizeof(*(arr)->data) * ((arr)->capacity - (arr)->length)); \
            }                                                                  \
        }                                                                      \
    } while (0)

/* Add element to array. Expands to a block statement. */
#define array_push_back(arr, elem) \
    do {                                                                       \
        array_increase_cap(arr);                                               \
        (arr)->data[(arr)->length++] = elem;                                   \
    } while (0)

/*
 * Copy elements from b to end of a, keeping b unchanged. Expands to a
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

/*
 * Remove and return last element in array. Expands to expression of
 * element type.
 */
#define array_pop_back(arr) \
    (arr)->data[--(arr)->length]

/*
 * Remove element at position, reducing length of array by 1.
 */
#define array_erase(arr, i) \
    do {                                                                       \
        assert(array_len(arr));                                                \
        memmove(                                                               \
            (arr)->data + i,                                                   \
            (arr)->data + i + 1,                                               \
            (array_len(arr) - i - 1) * sizeof(*(arr)->data));                  \
        (arr)->length--;                                                       \
    } while (0)

/*
 * Get element from array. Expands to expression, of type being
 * whatever is stored in the array.
 */
#define array_get(arr, i) \
    (arr)->data[i]

/*
 * Return length of the array. Expands to expression of unsigned integer
 * type.
 */
#define array_len(arr) \
    (arr)->length

/* Return last element of array. */
#define array_back(arr) \
    (array_get(arr, array_len(arr) - 1))

/* Reset element count to 0, but keep the capacity already allocated. */
#define array_empty(arr) \
    (arr)->length = 0

/* Resize array to hold at least given number of elements. */
#define array_realloc(arr, len) \
    do {                                                                       \
        if ((len) > (arr)->capacity) {                                         \
            (arr)->data = realloc((arr)->data, len * sizeof(*(arr)->data));    \
            (arr)->capacity = len;                                             \
            memset(                                                            \
                (arr)->data + (arr)->length,                                   \
                0,                                                             \
                sizeof(*(arr)->data) * ((arr)->capacity - array_len(arr)));    \
        }                                                                      \
    } while (0)

/* Set all elements of array to zero using memset. */
#define array_zero(arr) \
    do {                                                                       \
        if (array_len(arr)) {                                                  \
            memset((arr)->data, 0, array_len(arr) * sizeof(*(arr)->data));     \
        }                                                                      \
    } while (0)

/*
 * Free allocated memory, making the array empty. Expands to a block
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
