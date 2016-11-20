#include "abi.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

static int has_unaligned_fields(const struct typetree *type)
{
    int i;
    const struct member *mem;
    assert(is_struct_or_union(type));

    type = unwrapped(type);
    for (i = 0; i < nmembers(type); ++i) {
        mem = get_member(type, i);
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
    return PC_SSE;
}

static struct param_class flatten(
    struct param_class pc,
    const struct typetree *type,
    int offset)
{
    int i = offset / 8;
    const struct member *mb;

    switch (type->type) {
    case T_REAL:
        pc.eightbyte[i] = combine(pc.eightbyte[i], PC_SSE);
        break;
    case T_UNSIGNED:
    case T_SIGNED:
    case T_POINTER:
        pc.eightbyte[i] = combine(pc.eightbyte[i], PC_INTEGER);
        break;
    case T_STRUCT:
    case T_UNION:
        type = unwrapped(type);
        for (i = 0; i < nmembers(type); ++i) {
            mb = get_member(type, i);
            pc = flatten(pc, mb->type, mb->offset + offset);
        }
        break;
    default:
        assert(type->type == T_ARRAY);
        for (i = 0; i < type->size / size_of(type->next); ++i) {
            pc = flatten(pc, type->next, i * size_of(type->next) + offset);
        }
        break;
    }

    return pc;
}

static struct param_class merge(struct param_class pc)
{
    int i, sseup = 0, memory = 0;

    for (i = 0; i < 4 && pc.eightbyte[i] != PC_NO_CLASS; ++i) {
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

struct param_class classify(const struct typetree *type)
{
    struct param_class pc = {{PC_NO_CLASS}};
    assert(type->type != T_FUNCTION);

    if (is_integer(type) || is_pointer(type)) {
        pc.eightbyte[0] = PC_INTEGER;
    } else if (is_double(type) || is_float(type)) {
        pc.eightbyte[0] = PC_SSE;
    } else if (
        EIGHTBYTES(type) <= 4 &&
        is_struct_or_union(type) &&
        !has_unaligned_fields(type))
    {
        pc = flatten(pc, type, 0);
        pc = merge(pc);
    } else if (!is_void(type)) {
        pc.eightbyte[0] = PC_MEMORY;
    }

    return pc;
}

int sym_alignment(const struct symbol *sym)
{
    int align = type_alignment(&sym->type);
    if (is_array(&sym->type) && align < 16) {
        /*
         * A local or global array variable of at least 16 bytes should
         * have alignment of 16.
         */
        align = 16;
    }

    return align;
}

void dump_classification(struct param_class pc, const struct typetree *type)
{
    int i;
    printf("TYPE: ");
    fprinttype(stdout, type);
    printf("\n");
    printf("CLASS: %lu eightbyte\n", EIGHTBYTES(type));
    if (pc.eightbyte[0] == PC_MEMORY) {
        printf("\tMEMORY\n");
    } else {
        for (i = 0; i < EIGHTBYTES(type); ++i) {
            printf("\t%s\n", pc.eightbyte[i] == PC_INTEGER ? "INTEGER" : "SSE");
        }
    }
}
