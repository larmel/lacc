#include <lacc/string.h>

#include <ctype.h>
#include <string.h>

static int printchar(FILE *stream, char c)
{
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
    case '"':
        return fprintf(stream, "\\\"");
    default:
        return fprintf(stream, "\\0%02o", c);
    }
}

int fprintstr(FILE *stream, String str)
{
    int n, i;

    putc('"', stream);
    for (n = 0, i = 0; i < str.len; ++i)
        n += printchar(stream, str_raw(str)[i]);

    putc('"', stream);

    return n + 2;
}

String str_init(const char *str)
{
    String s;

    s.len = strlen(str);
    if (s.len < SHORT_STRING_LEN) {
        memcpy(s.a.str, str, s.len);
        s.a.str[s.len] = '\0';
    } else {
        s.p.str = str;
    }

    return s;
}

int str_cmp(String s1, String s2)
{
    if (s1.len != s2.len) {
        return 1;
    }

    if (s1.len < SHORT_STRING_LEN) {
        return memcmp(s1.a.str, s2.a.str, s1.len);
    }

    return memcmp(s1.p.str, s2.p.str, s1.len);
}
