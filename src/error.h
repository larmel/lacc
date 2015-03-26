#ifndef ERROR_H
#define ERROR_H

#include <stddef.h>

extern unsigned errors;

/* Error reporting, can be called from any component */
void error(const char *, ...);

/* Variadic macros not supported in C89, assume always used with one arg. */
#define internal_error(s, m) \
	do { \
		extern size_t line_number; \
		extern const char *filename; \
    	fprintf(stderr, "(%s, %ld) internal error in %s, line %d: ", filename, line_number, __FILE__, __LINE__); \
    	fprintf(stderr, s, m); \
    	fprintf(stderr, "\n"); \
	} while (0);


#endif
