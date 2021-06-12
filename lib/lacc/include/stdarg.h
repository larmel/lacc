#ifndef _STDARG_H
#define _STDARG_H

/*
 * Variable argument list is a builtin type defined by the compiler,
 * both for gcc and lacc. Alias magic internal symbol to va_list.
 */
typedef __builtin_va_list va_list;

/*
 * Bug in glibc, requiring this symbol.
 */
typedef __builtin_va_list __gnuc_va_list;

/*
 * va_start and va_arg are handled as compiler builtins, intercepted
 * during parsing. va_end is a no-op, no cleanup is required.
 */
#define va_start(list, arg) __builtin_va_start(list, arg)
#define va_arg(list, type) __builtin_va_arg(list, type)
#define va_end(list)

/* va_copy is introduced with C99. */
#if __STDC_VERSION__ >= 199901L
# define va_copy(dst, src) (*(dst) = *(src))
#endif

#endif
