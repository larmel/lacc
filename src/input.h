#ifndef INPUT_H
#define INPUT_H

#include <stddef.h>

/* Called by driver to initialize root directory, and set up required state in
 * the preprocessor. */
void init(char *);

/* Paths specified with -I, append to list of directories to search when
 * resolving includes. */
void add_include_search_path(const char *);

/* Push new include file. */
void include_file(const char *);
void include_system_file(const char *);

/* Yield next line ready for further preprocessing. Comments and all-whitespace
 * lines are removed. */
size_t getprepline(char **);

/* Expose global state to other components. */
extern size_t line_number;
extern const char *filename;

#endif
