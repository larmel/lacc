#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include <lacc/ir.h>

#include <stdio.h>

INTERNAL void set_compile_target(FILE *stream, const char *file)
{
    printf("Compiling for ARM!\n");
}

INTERNAL int compile(struct definition *def)
{
    return 0;
}

INTERNAL int declare(const struct symbol *sym)
{
    return 0;
}

INTERNAL void flush(void)
{
}

INTERNAL void finalize(void)
{
}
