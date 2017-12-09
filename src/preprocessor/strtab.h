#ifndef STRTAB_H
#define STRTAB_H

#include <lacc/string.h>
#include <lacc/token.h>
#include <lacc/type.h>

#include <stddef.h>

/*
 * Register a string and store it internally, allocating a new copy if
 * needed. Manages memory ownership for all string constants used at
 * runtime.
 */
String str_register(const char *str, size_t len);

/* Concatenate two strings together. */
String str_cat(String a, String b);

/*
 * Produce a quoted string with escape sequences for non-printable
 * characters, safe to output.
 */
String str_decoded(String str);

/* Create string represenation of character literal. */
String str_char(char c);

/* Create string representation of numeric constant. */
String str_number(union value val, Type type);

#endif
