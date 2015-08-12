#ifndef INPUT_H
#define INPUT_H

#include <stdio.h>
#include <stddef.h>

struct source {
    FILE *file;
    const char *name;
    const char *directory;
    const char *path;
    int line;
};

/* Initialize with root file name, and store relative path to resolve later
 * includes. Default to stdin.
 */
void init(char *);

/* Paths specified with -I, append to list of directories to search when
 * resolving includes.
 */
void add_include_search_path(const char *);

/* Push new include file.
 */
void include_file(const char *);
void include_system_file(const char *);

/* Yield next line ready for further preprocessing. Comments and all-whitespace
 * lines are removed.
 */
int getprepline(char **);

/* Expose global state to other components.
 */
extern struct source current_file;

#endif
