#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include <lacc/string.h>

#include <ctype.h>
#include <string.h>

static int printchar(FILE *stream, char ch)
{
    int c = (unsigned char) ch;
    if (isprint(c) && c != '"' && c != '\\') {
        putc(c, stream);
        return 1;
    }

    switch (c) {
    case '\b':
        return fprintf(stream, "\\b");
    case '\t':
        return fprintf(stream, "\\t");
    case '\n':
        return fprintf(stream, "\\n");
    case '\f':
        return fprintf(stream, "\\f");
    case '\r':
        return fprintf(stream, "\\r");
    case '\\':
        return fprintf(stream, "\\\\");
    case '\"':
        return fprintf(stream, "\\\"");
    default:
        return fprintf(stream, "\\%03o", c);
    }
}

INTERNAL int fprintstr(FILE *stream, String str)
{
    int n, i;
    const char *raw;

    raw = str_raw(str);
    putc('"', stream);
    for (n = 0, i = 0; i < str.len; ++i) {
        n += printchar(stream, raw[i]);
    }

    putc('"', stream);
    return n + 2;
}

INTERNAL String str_init(const char *str)
{
    String s = {0};

    s.len = strlen(str);
    if (s.len < SHORT_STRING_LEN) {
        memcpy(s.a.str, str, s.len);
    } else {
        s.p.str = str;
    }

    return s;
}

INTERNAL int str_cmp(String s1, String s2)
{
    long *a, *b;
    if (s1.len != s2.len) {
        return 1;
    }

    if (s1.len < SHORT_STRING_LEN) {
        a = (long *) ((void *) &s1);
        b = (long *) ((void *) &s2);
        return a[0] != b[0] || a[1] != b[1];
    }

    return memcmp(s1.p.str, s2.p.str, s1.len);
}
