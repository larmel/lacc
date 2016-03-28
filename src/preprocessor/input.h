#ifndef INPUT_H
#define INPUT_H

#include <stdio.h>
#include <stddef.h>

struct source {
    FILE *file;

    /* Total capacity of the line buffer is represented by size. The
     * number of characters already handled, a prefix, is 'processed'.
     * Read is the number of valid characters in the buffer. The
     * processed count grows on successive calls towards the read
     * number. When all read characters are processed, or the remaining
     * interval between (processed, read) does not contain a full line,
     * rewind the buffer, or increase if necessary. */
    char *buffer;
    size_t size, processed, read;

    /* Full path, or relative to invocation directory. */
    const char *path;

    /* Number of characters into path occupied by directory, not
     * including the last slash. */
    int dirlen;

    /* Current line. */
    int line;
};

/* Initialize with root file name, and store relative path to resolve
 * later includes. Passing NULL defaults to taking input from stdin.
 */
void init(const char *);

/* Paths specified with -I, append to list of directories to search when
 * resolving includes.
 */
void add_include_search_path(const char *);

/* Push new include file.
 */
void include_file(const char *);
void include_system_file(const char *);

/* Yield next line ready for further preprocessing. Comments and all-
 * whitespace lines are removed.
 */
int getprepline(char **);

/* Expose global state to other components.
 */
extern struct source current_file;

#endif
