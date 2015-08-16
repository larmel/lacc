#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* strdup, snprintf */
#endif
#include "string.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static int count;
static struct {
    const char *label;
    const char *string;
} *strings;

static const char *create_label(void)
{
    static unsigned int n;
    /* Integer (32 bit) can be at most 10 digits. Leave 3 for constant prefix,
     * and one for trailing null byte. */
    char *name = calloc(14, sizeof(*name));
    snprintf(name, 14, ".LC%d", n++);
    return name;
}

const char *strlabel(const char *s)
{
    int i;
    const char *label;
    static int capacity;

    for (i = 0; i < count; ++i) {
        if (!strcmp(strings[i].string, s)) {
            return strings[i].label;
        }
    }

    if (count == capacity) {
        capacity += 16;
        strings = realloc(strings, sizeof(*strings) * capacity);
    }

    label = create_label();

    strings[count].string = strdup(s);
    strings[count].label = label;
    count++;

    return label;
}

void output_string(FILE *stream, const char *str)
{
    char c;

    while ((c = *str++) != '\0') {
        if (isprint(c) && c != '"' && c != '\\')
            putc(c, stream);
        else
            fprintf(stream, "\\x%x", (int) c);
    }
}

/* Assemble strings readonly data section, GNU assembler syntax.
 */
void output_strings(FILE *stream)
{
    int i;
    if (count) {
        fprintf(stream, "\t.section .rodata\n");
        for (i = 0; i < count; ++i) {
            fprintf(stream, "%s:\n", strings[i].label);
            fprintf(stream, "\t.string \"");
            output_string(stream, strings[i].string);
            fprintf(stream, "\"\n");
        }
    }
}
