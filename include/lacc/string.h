#ifndef STRING_H
#define STRING_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include <stddef.h>
#include <stdio.h>

#define SHORT_STRING_LEN 15
#define SHORT_STRING_INIT(s) {{s, SHORT_STRING_LEN - (sizeof(s) - 1)}}
#define IS_SHORT_STRING(s) ((s).small.cap >= 0)
#define MAX_STRING_LEN 0x00ffffffffffffff

/*
 * Compact representation of strings, such as identifiers and literals.
 * Optimize for short lengths, storing it inline in the object itself.
 * This type fits in 2 eightbytes.
 *
 * Small strings use up to the first 15 bytes as the string value, and
 * the last byte is remaining capacity. This is a very clever idea that
 * is inspired by FBString. When the string is full, the remaining
 * capacity is zero, and works dual purpose as null terminator. This
 * also allows storing small strings with embedded null characters.
 *
 * Large strings (more than 15 chars) are stored as a pointer followed
 * by a length of 7 bytes. We rely on little endian for length access to
 * work by just masking away the last byte, which contain a negative
 * value to signal a large string.
 */
typedef union {
    struct {
        char buf[SHORT_STRING_LEN];
        char cap;
    } small;
    struct {
        const char *ptr;
        size_t len;
    } large;
} String;

/*
 * Get pointer to plain C string representation. This depends on the
 * type of string, whether it is small or large.
 *
 * Note that this cannot be implemented as a function call, since the
 * resulting pointer is only valid for the same duration as the string
 * argument.
 */
#define str_raw(s) (IS_SHORT_STRING(s) ? (s).small.buf : (s).large.ptr)

/* Compute length of string. */
INTERNAL size_t str_len(String s);

/* Return an empty string. */
INTERNAL String str_empty(void);

/* Compare string length to 0. */
INTERNAL int str_is_empty(String s);

/* Write length and buffer to pre-allocated string object. */
INTERNAL void str_set(String *s, const char *str, size_t len);

/* Compare two strings for equality. */
INTERNAL int str_eq(String s1, String s2);

/* Return 1 iff string contains given character. */
INTERNAL int str_has_chr(String s, char c);

/* Hash of string. */
INTERNAL int str_hash(String str);

/*
 * Output string to stream, in safe encoding for textual assembly or as
 * plain C code.
 */
INTERNAL int fprintstr(FILE *stream, String str);

#endif
