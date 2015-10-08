#include "abi.h"
#include "core/error.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

enum reg param_int_reg[] = {DI, SI, DX, CX, R8, R9};
enum reg ret_int_reg[] = {AX, DX};

static int has_unaligned_fields(const struct typetree *t)
{
    int i;
    if (is_struct_or_union(t)) {
        t = unwrapped(t);
        for (i = 0; i < t->n; ++i)
            if (t->member[i].offset % size_of(t->member[i].type))
                return 1;
    }
    return 0;
}

static enum param_class combine(enum param_class a, enum param_class b)
{
    if (a == b) return a;
    if (a == PC_NO_CLASS) return b;
    if (b == PC_NO_CLASS) return a;
    if (a == PC_MEMORY || b == PC_MEMORY) return PC_MEMORY;
    if (a == PC_INTEGER || b == PC_INTEGER) return PC_INTEGER;
    return PC_SSE;
}

/* Traverse type tree depth first, calculating parameter classification to use
 * for each eightbyte.
 */
static void flatten(enum param_class *l, const struct typetree *t, int offset)
{
    int i = offset / 8;

    switch (t->type) {
    case T_REAL:
    case T_UNSIGNED:
    case T_SIGNED:
    case T_POINTER:
        /* All fields should be aligned, i.e. a type cannot span multiple eight
         * byte boundaries. */
        assert(size_of(t) <= 8 /*&& (offset + t->size) / 8 == idx*/);

        l[i] = combine(l[i], t->type == T_REAL ? PC_SSE : PC_INTEGER);
        break;
    case T_STRUCT:
    case T_UNION:
        t = unwrapped(t);
        for (i = 0; i < t->n; ++i) {
            flatten(l, t->member[i].type, t->member[i].offset + offset);
        }
        break;
    case T_ARRAY:
        internal_error("%s", "Not yet support for array parameters.");
        exit(1);
    default:
        assert(0);
    }
}

static int merge(enum param_class *l, int n)
{
    int i, has_sseup = 0;

    for (i = 0; i < n; ++i) {
        if (l[i] == PC_MEMORY) {
            return 1;
        } else if (l[i] == PC_SSEUP) {
            has_sseup = 1;
        }
    }

    if (n > 2 && (l[0] != PC_SSE || !has_sseup)) {
        return 1;
    }

    return 0;
}

/* Parameter classification as described in System V ABI (3.2.3), with some
 * simplifications.
 * Classify parameter as a series of eightbytes used for parameter passing and
 * return value. If the first element is not PC_MEMORY, the number of elements
 * in the list can be determined by N_EIGHTBYTES(t). 
 */
enum param_class *classify(const struct typetree *t)
{
    enum param_class *eb = calloc(1, sizeof(*eb));

    assert(t->type != T_FUNCTION);
    assert(t->type != T_VOID);

    if (is_integer(t) || is_pointer(t)) {
        *eb = PC_INTEGER;
    } else if (N_EIGHTBYTES(t) > 4 || has_unaligned_fields(t)) {
        *eb = PC_MEMORY;
    } else if (is_struct_or_union(t)) {
        int n = N_EIGHTBYTES(t);

        eb = realloc(eb, n * sizeof(*eb));
        memset(eb, 0, n * sizeof(*eb)); /* Initialize to NO_CLASS. */
        flatten(eb, t, 0);
        if (merge(eb, n)) {
            eb = realloc(eb, sizeof(*eb));
            *eb = PC_MEMORY;
        }
    } else {
        *eb = PC_MEMORY;
    }

    return eb;
}

/* Classify function call, required as separate from classifying signature
 * because of variable length argument list.
 */
enum param_class **
classify_call(const struct typetree **args,
              const struct typetree *ret,
              int n_args,
              enum param_class **out)
{
    int i,
        next_integer_reg = 0;

    enum param_class
        **params = calloc(n_args, sizeof(*params)),
        *res = calloc(1, sizeof(*res));

    /* Classify parameters and return value. */
    for (i = 0; i < n_args; ++i) {
        params[i] = classify(args[i]);
    }

    if (ret->type != T_VOID) {
        res = classify(ret);

        /* When return value is MEMORY, pass a pointer to stack as hidden first
         * argument. */
        if (*res == PC_MEMORY) {
            next_integer_reg = 1;
        }
    }

    /* Place arguments in registers from left to right, partitioned into
     * eightbyte slices. */
    for (i = 0; i < n_args; ++i) {
        if (*params[i] != PC_MEMORY) {
            int chunks = N_EIGHTBYTES(args[i]);

            /* Arguments are not partially passed on stack, so check that there
             * are enough registers available. */
            if (next_integer_reg + chunks <= 6) {
                next_integer_reg += chunks;
            } else {
                *params[i] = PC_MEMORY;
            }
        }
    }

    assert(out);

    *out = res;
    return params;
}

/* Classify parameters and return value of function type.
 */
enum param_class **
classify_signature(const struct typetree *func, enum param_class **out)
{
    int i;
    enum param_class **params;
    const struct typetree **args;

    assert(is_function(func));

    args = calloc(func->n, sizeof(*args));
    for (i = 0; i < func->n; ++i) {
        args[i] = func->member[i].type;
    }

    params = classify_call(args, func->next, func->n, out);
    free(args);

    return params;
}

int sym_alignment(const struct symbol *sym)
{
    int align = type_alignment(&sym->type);
    if (is_array(&sym->type) && align < 16) {
        /* A local or global array variable of at least 16 bytes should have
         * alignment of 16. */
        align = 16;
    }

    return align;
}

void dump_classification(const enum param_class *c, const struct typetree *t) {
    int i;

    printf("TYPE: %s\n", typetostr(t));
    printf("CLASS: %d eightbytes\n", N_EIGHTBYTES(t));
    for (i = 0; i < N_EIGHTBYTES(t) && (*c != PC_MEMORY || i < 1); ++i) {
        printf("\t%s\n", 
            c[i] == PC_INTEGER ? "INTEGER" :
            c[i] == PC_MEMORY ? "MEMORY" : "UNKNOWN");
    }
}
