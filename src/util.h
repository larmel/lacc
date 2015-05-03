#ifndef UTIL_H
#define UTIL_H

/* Output functions implementing init(), push_back() and destroy() for a given
 * name and type. For example: invoking with ('sym_list', struct symbol *) will
 * define functions sym_list_init(), sym_list_push_back(struct sym_list *) and
 * sym_list_destroy(struct sym_list *). */
#define DECLARE_LIST_IMPLEMENTATION(name, T) \
    struct name { \
        T *elem; \
        size_t length; \
        size_t cap; \
    }; \
    struct name * name ## _init(); \
    void name ## _destroy(struct name *); \
    void name ## _push_back(struct name *, T);

#define DEFINE_LIST_IMPLEMENTATION(name, T) \
    struct name * name ## _init() { \
        return calloc(1, sizeof(struct name)); \
    } \
    void name ## _destroy(struct name *lst) { \
        assert(lst); \
        if (lst->elem) { \
            free(lst->elem); \
            lst->elem = NULL; \
        } \
        free(lst); \
    } \
    void name ## _push_back(struct name *lst, T e) { \
        assert(lst); \
        lst->length++; \
        lst->elem = realloc(lst->elem, sizeof(*lst->elem) * lst->length); \
        lst->elem[lst->length - 1] = e; \
    }


#endif
