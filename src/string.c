#if _XOPEN_SOURCE < 700
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strdup, strndup */
#endif
#include "string.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static int count;
static int capacity;
static struct {
    const char *label;
    const char *string;
} *strings;

static const char *mklabel(void)
{
    static int n;
    static char name[16];

    snprintf(name, 10, ".LC%d", n++);

    return strndup(name, 15);
}

/* Return an existing, or generate a new unique label representing the provided
 * string. Labels are used verbatim for assembly tags.
 */
const char *strlabel(const char *s)
{
    int i;
    const char *label;

    for (i = 0; i < count; ++i) {
        if (!strcmp(strings[i].string, s)) {
            return strings[i].label;
        }
    }

    if (count == capacity) {
        capacity = capacity + 16;
        strings = realloc(strings, sizeof(*strings) * capacity);
    }

    label = mklabel();

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
