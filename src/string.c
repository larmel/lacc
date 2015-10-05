#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup, snprintf */
#endif
#include "string.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* List of all dynamically allocated strings produced.
 */
static size_t count;
static struct string {
    size_t length;
    char *label;
    char *string;
} *strings;

static void cleanup(void)
{
    size_t i;
    for (i = 0; i < count; ++i) {
        if (strings[i].label)
            free(strings[i].label);
        free(strings[i].string);
    }
    if (count)
        free(strings);
    count = 0;
    strings = NULL;
}

static char *create_label(void)
{
    static unsigned int n;

    /* Integer (32 bit) can be at most 10 digits. Leave 3 for constant prefix,
     * and one for trailing null byte. */
    char *name = calloc(14, sizeof(*name));
    snprintf(name, 14, ".LC%d", n++);
    return name;
}

static struct string *get_or_add(const char *s, size_t n)
{
    size_t i;
    static size_t capacity;

    for (i = 0; i < count; ++i)
        if (strings[i].length == n && !strncmp(strings[i].string, s, n))
            return &strings[i];

    /* There are going to be some strings in this program; register exit handler
     * to free memory in the end. */
    if (!count)
        atexit(cleanup);

    if (count == capacity) {
        capacity += 16;
        strings = realloc(strings, sizeof(*strings) * capacity);
    }

    strings[count].length = n;
    strings[count].string = strndup(s, n);
    strings[count].label = NULL;
    return &strings[count++];
}

const char *str_register(const char *s)
{
    struct string *str = get_or_add(s, strlen(s));
    return str->string;
}

const char *str_register_n(const char *s, size_t n)
{
    struct string *str = get_or_add(s, n);
    return str->string;
}

const char *strlabel(const char *s)
{
    struct string *str = get_or_add(s, strlen(s));
    if (!str->label)
        str->label = create_label();

    return str->label;
}

static void unescape(FILE *stream, int c)
{
    if (isprint(c) && c != '"' && c != '\\') {
        putc(c, stream);
        return;
    }

    switch (c) {
    case '\b': fprintf(stream, "\\b"); break;
    case '\t': fprintf(stream, "\\t"); break;
    case '\n': fprintf(stream, "\\n"); break;
    case '\f': fprintf(stream, "\\f"); break;
    case '\r': fprintf(stream, "\\r"); break;
    case '\\': fprintf(stream, "\\\\"); break;
    case '"': fprintf(stream, "\\\""); break;
    default:
        fprintf(stream, "\\0%02o", c);
        break;
    }
}

void output_string(FILE *stream, const char *str)
{
    char c;
    while ((c = *str++) != '\0') {
        unescape(stream, c);
    }
}

void output_strings(FILE *stream)
{
    size_t i;
    if (count)
        fprintf(stream, "\t.section .rodata\n");
    for (i = 0; i < count; ++i) {
        if (!strings[i].label)
            continue;
        fprintf(stream, "%s:\n", strings[i].label);
        fprintf(stream, "\t.string \"");
        output_string(stream, strings[i].string);
        fprintf(stream, "\"\n");
    }
}
