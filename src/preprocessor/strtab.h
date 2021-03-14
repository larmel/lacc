#ifndef STRTAB_H
#define STRTAB_H

#include <lacc/string.h>
#include <lacc/token.h>
#include <lacc/type.h>

#include <stddef.h>

/*
 * Register a string and store it internally as a singleton, allocating
 * a copy for each unique string.
 *
 * This is the only valid way of creating string objects, and it
 * guarantees that equality checks can be reduced to checking pointers
 * internally.
 */
INTERNAL String str_intern(const char *str, size_t len);

/* Concatenate two strings together, returning a new interned string. */
INTERNAL String str_cat(String a, String b);

/* Free memory used for string table. */
INTERNAL void strtab_reset(void);

#endif
