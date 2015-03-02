/* Manage string constants encountered in the translation unit. Strings declared
 * in plain text are by default read-only, except for when the string literal is
 * assigned to an array. Keep a global map of all strings, and remove duplicates
 * to save space.
 */

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


static int count;
static int capacity;
static struct strmap {
    const char *label;
    const char *string;
} *strings;

static const char *
mklabel()
{
    static int n;
    static char name[16];

    snprintf(name, 10, ".LC%d", n++);

    return strndup(name, 15);
}

static const char *
getoradd(const char *s)
{
    int i;
    const char *label;

    for (i = 0; i < count; ++i) {
        if (!strcmp(strings[i].string, s)) {
            return strings[i].label;
        }
    }

    if (count == capacity) {
        capacity += 16;
        strings = realloc(strings, sizeof(struct strmap) * capacity);
    }

    label = mklabel();

    strings[count].string = strdup(s);
    strings[count].label = label;
    count++;

    return label;
}

/* Return a label representing the provided string, i.e. '.S1'.
 */
const char *
string_constant_label(const char *s)
{
    assert(s);
    return getoradd(s);
}

static void
printstr(FILE *stream, const char *str)
{
    char c;

    while ((c = *str++) != '\0') {
        if (isprint(c) && c != '"' && c != '\\')
            putc(c, stream);
        else
            fprintf(stream, "\\x%x", (int) c);
    }
}

void
output_string(FILE *stream, const char *l)
{
    int i;

    for (i = 0; i < count; ++i) {
        if (!strcmp(strings[i].label, l)) {
            printstr(stream, strings[i].string);
            break;
        }
    }
}

void
output_strings(FILE *stream)
{
    if (count) {
        int i;

        fprintf(stream, "\t.section .rodata\n");
        for (i = 0; i < count; ++i) {
            fprintf(stream, "%s:\n", strings[i].label);
            fprintf(stream, "\t.string \"");
            printstr(stream, strings[i].string);
            fprintf(stream, "\"\n");
        }
    }
}
