#ifndef _STDDEF_H
#define _STDDEF_H

typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __WCHAR_TYPE__ wchar_t;

#define NULL ((void *) 0)
#define offsetof(T, field) ((size_t) &((T *) 0)->field)

#endif
