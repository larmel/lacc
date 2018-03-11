#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "abi.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

static int has_unaligned_fields(Type type)
{
    int i;
    const struct member *mem;
    assert(is_struct_or_union(type));

    for (i = 0; i < nmembers(type); ++i) {
        mem = get_member(type, i);
        assert(size_of(mem->type));
        if (mem->offset % size_of(mem->type)) {
            return 1;
        }
    }

    return 0;
}

static unsigned char combine(unsigned char a, unsigned char b)
{
    if (a == b) return a;
    if (a == PC_NO_CLASS) return b;
    if (b == PC_NO_CLASS) return a;
    if (a == PC_MEMORY || b == PC_MEMORY) return PC_MEMORY;
    if (a == PC_INTEGER || b == PC_INTEGER) return PC_INTEGER;
    if (a == PC_X87 || b == PC_X87) return PC_MEMORY;
    if (a == PC_X87UP || b == PC_X87UP) return PC_MEMORY;
    return PC_SSE;
}

static struct param_class flatten(
    struct param_class pc,
    Type type,
    int offset)
{
    int i = offset / 8;
    Type next;
    const struct member *mb;

    switch (type_of(type)) {
    default: assert(0);
    case T_FLOAT:
    case T_DOUBLE:
        pc.eightbyte[i] = combine(pc.eightbyte[i], PC_SSE);
        break;
    case T_LDOUBLE:
        pc.eightbyte[i] = combine(pc.eightbyte[i], PC_X87);
        if (i < 3) {
            pc.eightbyte[i + 1] = combine(pc.eightbyte[i + 1], PC_X87UP);
        }
        break;
    case T_CHAR:
    case T_INT:
    case T_SHORT:
    case T_LONG:
    case T_POINTER:
        pc.eightbyte[i] = combine(pc.eightbyte[i], PC_INTEGER);
        break;
    case T_STRUCT:
    case T_UNION:
        for (i = 0; i < nmembers(type); ++i) {
            mb = get_member(type, i);
            pc = flatten(pc, mb->type, mb->offset + offset);
        }
        break;
    case T_ARRAY:
        next = type_next(type);
        for (i = 0; i < size_of(type) / size_of(next); ++i) {
            pc = flatten(pc, next, i * size_of(next) + offset);
        }
        break;
    }

    return pc;
}

static struct param_class merge(struct param_class pc, int n)
{
    int i, sseup = 0, memory = 0;

    for (i = 0; i < n; ++i) {
        switch (pc.eightbyte[i]) {
        case PC_X87UP:
            if (i && pc.eightbyte[i - 1] == PC_X87)
                break;
        case PC_MEMORY:
            memory = 1;
            break;
        case PC_SSEUP:
            sseup = 1;
        default:
            break;
        }
    }

    memory = memory || (i > 2 && (pc.eightbyte[0] != PC_SSE || !sseup));
    if (memory) {
        pc.eightbyte[0] = PC_MEMORY;
    }

    return pc;
}

INTERNAL struct param_class classify(Type type)
{
    struct param_class pc = {PC_NO_CLASS};
    assert(type_of(type) != T_FUNCTION);

    if (is_integer(type) || is_pointer(type)) {
        pc.eightbyte[0] = PC_INTEGER;
    } else if (is_double(type) || is_float(type)) {
        pc.eightbyte[0] = PC_SSE;
    } else if (is_long_double(type)) {
        pc.eightbyte[0] = PC_X87;
        pc.eightbyte[1] = PC_X87UP;
    } else if (EIGHTBYTES(type) <= 4
        && is_struct_or_union(type)
        && !is_flexible(type)
        && !has_unaligned_fields(type))
    {
        pc = flatten(pc, type, 0);
        pc = merge(pc, EIGHTBYTES(type));
    } else if (!is_void(type)) {
        pc.eightbyte[0] = PC_MEMORY;
    }

    return pc;
}

INTERNAL int sym_alignment(const struct symbol *sym)
{
    int align = type_alignment(sym->type);
    if (is_array(sym->type) && align < 16) {
        /*
         * A local or global array variable of at least 16 bytes should
         * have alignment of 16.
         */
        align = 16;
    }

    return align;
}

#if !NDEBUG
void dump_classification(struct param_class pc, Type type)
{
    int i;
    printf("TYPE: ");
    fprinttype(stdout, type, NULL);
    printf("\n");
    printf("CLASS: %lu eightbyte\n", EIGHTBYTES(type));
    if (pc.eightbyte[0] == PC_MEMORY) {
        printf("\tMEMORY\n");
    } else {
        for (i = 0; i < EIGHTBYTES(type); ++i) {
            printf("\t%s\n",
                pc.eightbyte[i] == PC_INTEGER ? "INTEGER" :
                pc.eightbyte[i] == PC_SSE ? "SSE" :
                pc.eightbyte[i] == PC_X87 ? "X87" :
                pc.eightbyte[i] == PC_X87UP ? "X87UP" :
                pc.eightbyte[i] == PC_NO_CLASS ? "NO_CLASS" : "<invalid>");
        }
    }
}
#endif
