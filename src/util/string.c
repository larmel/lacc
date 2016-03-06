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

int fprintstr(FILE *stream, struct string str)
{
    int n, i;

    putc('"', stream);
    for (n = 0, i = 0; i < str.len; ++i)
        n += printchar(stream, str.str[i]);

    putc('"', stream);

    return n + 2;
}

struct string str_init(const char *str)
{
    struct string s = {0};
    s.str = str;
    s.len = strlen(str);
    return s;
}

int str_cmp(struct string s1, struct string s2)
{
    return (s1.len != s2.len) || memcmp(s1.str, s2.str, s1.len);
}
