#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include <lacc/context.h>
#include <lacc/string.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
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
    size_t len;
    const char *raw;

    raw = str_raw(str);
    len = str_len(str);
    putc('"', stream);
    for (n = 0, i = 0; i < len; ++i) {
        n += printchar(stream, raw[i]);
    }

    putc('"', stream);
    return n + 2;
}

INTERNAL String str_empty(void)
{
    String s = SHORT_STRING_INIT("");
    return s;
}

INTERNAL size_t str_len(String s)
{
    if (IS_SHORT_STRING(s)) {
        return SHORT_STRING_LEN - s.small.cap;
    }

    return s.large.len & MAX_STRING_LEN;
}

INTERNAL int str_is_empty(String s)
{
    return str_len(s) == 0;
}

INTERNAL int str_eq(String s1, String s2)
{
    return s1.large.ptr == s2.large.ptr && s1.large.len == s2.large.len;
}

INTERNAL int str_has_chr(String s, char c)
{
    int i;
    size_t len;
    const char *str;

    len = str_len(s);
    str = str_raw(s);
    for (i = 0; i < len; ++i) {
        if (str[i] == c) {
            return 1;
        }
    }

    return 0;
}

INTERNAL int str_hash(String str)
{
    int hash, i;
    union {
        String s;
        int d[4];
    } p;

    p.s = str;
    assert(sizeof(str) == sizeof(p.d));
    for (hash = 5381, i = 0; i < 4; ++i) {
        hash = ((hash << 5) + hash) + p.d[i];
    }

    return hash;
}
