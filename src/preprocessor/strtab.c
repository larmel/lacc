#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

static struct hash_table strtab;

/* Buffer used to concatenate strings before registering them. */
static char *catbuf;
static size_t catlen;

/*
 * Every unique string encountered, being identifiers or literals, is
 * kept for the lifetime of the program. To save allocations, store the
 * raw string buffer in the same allocation as the struct.
 *
 *  _________ String ________    ________ const char [] ________
 * |                          | |                               |
 * [ <len> | <ptr to data>    ] [ 'H', 'e', 'l', 'l', 'o', '\0' ]
 */
static void *str_hash_add(void *ref)
{
    String *s;
    char *buffer;
    unsigned short l;

    s = (String *) ref;
    l = s->p.len;
    buffer = malloc(sizeof(String) + l + 1);
    buffer[sizeof(String) + l] = '\0';
    memcpy(buffer + sizeof(String), s->p.str, l);
    s = (String *) buffer;
    s->p.str = buffer + sizeof(*s);
    s->p.len = l;
    return s;
}

static String str_hash_key(void *ref)
{
    String *str = (String *) ref;
    return *str;
}

static void strtab_free(void)
{
    hash_destroy(&strtab);
    free(catbuf);
}

String str_register(const char *str, size_t len)
{
    static int initialized;
    String data = {0}, *ref;
    assert(len >= 0);

    if (len < SHORT_STRING_LEN) {
        memcpy(data.a.str, str, len);
        data.a.len = len;
        ref = &data;
    } else {
        if (!initialized) {
            hash_init(
                &strtab,
                STRTAB_SIZE,
                str_hash_key,
                str_hash_add,
                free);

            atexit(strtab_free);
            initialized = 1;
        }
        data.p.str = str;
        data.p.len = len;
        ref = hash_insert(&strtab, &data);
    }

    return *ref;
}

String str_cat(String a, String b)
{
    size_t len;

    len = a.len + b.len;
    if (len > catlen) {
        catlen = len;
        catbuf = realloc(catbuf, catlen);
    }

    memcpy(catbuf, str_raw(a), a.len);
    memcpy(catbuf + a.len, str_raw(b), b.len);
    return str_register(catbuf, len);
}

static int is_printable(int c)
{
    unsigned char u = (unsigned char) c;
    return u == c && isprint(u) && c != '"' && c != '\\' && c != '\'';
}

/*
 * Give an upper bound on how many characters is necessary to serialize
 * escaped string.
 */
static size_t length_estimate(String s)
{
    int i;
    size_t n = 0;
    const char *raw;

    raw = str_raw(s);
    for (i = 0; i < s.len; ++i) {
        if (!is_printable(raw[i])) {
            n += 1;
        }
    }

    return n * 3 + s.len + 3;
}

static size_t write_char_escaped(char c, char *buf, size_t i)
{
    if (is_printable(c)) {
        buf[i++] = c;
    } else {
        buf[i++] = '\\';
        switch (c) {
        case '\a':
            buf[i++] = 'a';
            break;
        case '\b':
            buf[i++] = 'b';
            break;
        case '\t':
            buf[i++] = 't';
            break;
        case '\n':
            buf[i++] = 'n';
            break;
        case '\v':
            buf[i++] = 'v';
            break;
        case '\f':
            buf[i++] = 'f';
            break;
        case '\r':
            buf[i++] = 'r';
            break;
        case '\0':
            buf[i++] = '0';
            break;
        case '\\':
        case '\"':
        case '\'':
            buf[i++] = c;
            break;
        default:
            sprintf(buf + i, "%03o", (unsigned char) c);
            i += 3;
            break;
        }
    }

    return i;
}

String str_decoded(String str)
{
    const char *raw;
    char *buf;
    size_t len, i, j = 0;

    raw = str_raw(str);
    len = length_estimate(str);
    buf = calloc(len, sizeof(*buf));

    buf[j++] = '\"';
    for (i = 0; i < str.len; ++i) {
        assert(j < len + 2);
        j = write_char_escaped(raw[i], buf, j);
    }

    buf[j++] = '\"';
    str = str_register(buf, j);
    free(buf);
    return str;
}

String str_char(char c)
{
    static char buf[16];
    size_t i = 0;

    buf[i++] = '\'';
    i = write_char_escaped(c, buf, i);
    buf[i++] = '\'';
    return str_register(buf, i);
}

String str_number(union value val, Type type)
{
    static char buf[512];
    size_t len;

    if (is_unsigned(type)) {
        if (size_of(type) == 8) {
            len = sprintf(buf, "%luul", val.u);
        } else {
            len = sprintf(buf, "%luu", val.u);
        }
    } else if (is_signed(type)) {
        if (size_of(type) == 8) {
            len = sprintf(buf, "%ldl", val.i);
        } else {
            len = sprintf(buf, "%ld", val.i);
        }
    } else if (is_float(type)) {
        len = sprintf(buf, "%ff", val.f);
    } else if (is_double(type)) {
        len = sprintf(buf, "%f", val.d);
    } else {
        assert(is_long_double(type));
        len = sprintf(buf, "%Lf", val.ld);
    }

    return str_register(buf, len);
}
