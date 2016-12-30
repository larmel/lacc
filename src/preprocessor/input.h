#ifndef INPUT_H
#define INPUT_H

#include <lacc/string.h>

/*
 * Initialize with root file name, and store relative path to resolve
 * later includes. Passing NULL defaults to taking input from stdin.
 */
void init(const char *);

/*
 * Paths specified with -I, append to list of directories to search when
 * resolving includes.
 */
void add_include_search_path(const char *);

/* Push new include file. */
void include_file(const char *);
void include_system_file(const char *);

/*
 * Yield next line ready for further preprocessing. Joins continuations,
 * and replaces comments with a single space. Line implicitly ends with
 * a single newline character ('\n'), but it is not included.
 */
char *getprepline(void);

/* Path of file and line number that was last read. */
extern String current_file_path;
extern int current_file_line;

#endif
