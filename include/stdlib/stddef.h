#ifndef _STDDEF_H
#define _STDDEF_H

typedef unsigned long size_t;
typedef signed long ptrdiff_t;
typedef signed int wchar_t;

#define NULL ((void *) 0)
#define offsetof(T, field) ((size_t) &((T *) 0)->field)

#endif
