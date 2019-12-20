#ifndef INPUT_H
#define INPUT_H

#include <lacc/string.h>

/*
 * Options controlling how to emit make dependencies.
 */
struct dependency_config {
    unsigned int skip_system_headers : 1;
    unsigned int phony_targets : 1;
    unsigned int accept_missing_headers : 1;
    unsigned int suppress_preprocessor_output : 1;
    unsigned int generate_file_name : 1;
};

EXTERNAL struct dependency_config dependency_config;

/*
 * Initialize with root file name, and store relative path to resolve
 * later includes. Passing NULL defaults to taking input from stdin.
 */
INTERNAL void set_input_file(const char *);

/* Free resources used for reading input. */
INTERNAL void input_finalize(void);

/*
 * Paths specified with -I, append to list of directories to search when
 * resolving includes.
 */
INTERNAL int add_include_search_path(const char *);

/* Push new include file. */
INTERNAL void include_file(const char *);
INTERNAL void include_system_file(const char *);

/* Add file to be included before the main source file. */
INTERNAL int add_include_file(const char *path);

/*
 * Yield next line ready for further preprocessing. Joins continuations,
 * and replaces comments with a single space. Line implicitly ends with
 * a single newline character ('\n'), but it is not included.
 */
INTERNAL char *getprepline(void);

/* Output dependencies to file. */
INTERNAL void write_makefile(FILE *f, const char *target);

/* Path of file and line number that was last read. */
EXTERNAL String current_file_path;
EXTERNAL int current_file_line;

#endif
