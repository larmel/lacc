#if _XOPEN_SOURCE < 600
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 600 /* isblank */
#endif
#include "error.h"
#include "input.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* Globally exposed for diagnostics info and default macro values.
 */
struct source current_file;

/* List of directories to search on resolving include directives.
 */
static const char **search_path;
static size_t search_path_count;

/* Keep stack of file descriptors as resolved by includes. Push and pop from the
 * end of the list.
 */
static struct source *src_stack;
static size_t src_count;

static int pop(void)
{
    if (src_count) {
        struct source *source = &src_stack[--src_count];
        if (source->file != stdin) {
            fclose(source->file);
        }
        memset(source, 0, sizeof(*source));
        if (src_count) {
            current_file = src_stack[src_count - 1];
            return 1;
        }
    }
    return EOF;
}

static void push(struct source source)
{
    src_count++;
    src_stack = realloc(src_stack, src_count * sizeof(*src_stack));
    src_stack[src_count - 1] = source;
}

static char *create_path(const char *dir, const char *name)
{
    char *path = malloc(strlen(dir) + 1 + strlen(name) + 1);
    strcpy(path, dir);
    strcat(path, "/");
    strcat(path, name);
    return path;
}

static void include_file_internal(const char *name, int incurrent)
{
    struct source source = {0};

    source.name = name;

    /* First check in current directory. */
    if (incurrent) {
        source.path = create_path(current_file.directory, name);
        source.file = fopen(source.path, "r");
        source.directory = current_file.directory;
    }

    /* Go through list of include paths. */
    if (!source.file) {
        int i = search_path_count - 1;
        if (!incurrent) {
            i--; /* skip invocation dir for system files. */
        }
        for (; i >= 0 && !source.file; --i) {
            if (source.path) {
                free((void *) source.path);
                source.path = NULL;
            }
            source.path = create_path(search_path[i], name);
            source.file = fopen(source.path, "r");
            source.directory = search_path[i];
        }
    }

    if (!source.file) {
        error("Unable to resolve include file %s.", name);
        exit(1);
    }

    current_file = source;
    push(source);
}

void include_file(const char *name)
{
    include_file_internal(name, 1);
}

void include_system_file(const char *name)
{
    include_file_internal(name, 0);
}

/* Clean up all dynamically allocated resources.
 */
static void finalize()
{
    assert(src_stack);
    while (pop() != EOF)
        ;
    free(src_stack);
}

void add_include_search_path(const char *path)
{
    /* For the first time, add default search paths at the bottom. */
    if (!search_path) {
        search_path = calloc(3, sizeof(*search_path));
        add_include_search_path("/usr/include");
        add_include_search_path("/usr/local/include");
    }

    search_path_count++;
    search_path = realloc(search_path,
        search_path_count * sizeof(*search_path));
    search_path[search_path_count - 1] = path;
}

void init(char *path)
{
    struct source source = {0};

    source.file = stdin;
    source.path = "<stdin>";
    source.name = "<stdin>";
    source.directory = ".";
    source.line = 0;

    if (path) {
        char *sep;

        source.name = path;
        sep = strrchr(path, '/');
        if (sep) {
            *sep = '\0';
            source.name = sep + 1;
            source.directory = path;
        }

        source.path = create_path(source.directory, source.name);
        source.file = fopen(source.path, "r");
        if (!source.file) {
            error("Unable to open file %s.", source.path);
            exit(1);
        }
    }

    current_file = source;
    push(source);
    add_include_search_path(source.directory);
    atexit(finalize);
}

/* Read characters from stream and assemble a line.
 *
 *  - Keep track of and remove comments.
 *  - Join lines ending with '\'.
 *
 * Increment line counter in fnt structure for each line consumed. Ignore all-
 * whitespace lines.
 */
static int getcleanline(char **lineptr, size_t *n, struct source *fn)
{
    enum { NORMAL, COMMENT } state = 0;
    int c, next;            /* Return value of getc. */
    int i = 0,              /* Number of chars written to output buffer. */
        last = '\0';        /* Last non-whitespace character consumed. */
    assert(fn);

    /* Need to have room for terminating '\0' byte. */
    if (!*n) {
        *n = 1;
        *lineptr = calloc(1, sizeof(**lineptr));
    }

    while ((c = getc(fn->file)) != EOF) {
        /* Line continuation */
        if (c == '\\') {
            next = getc(fn->file);
            if (next == EOF) {
                error("Invalid end of file after line continuation.");
                exit(1);
            }
            if (next == '\n') {
                fn->line++;
                continue;
            }
            ungetc(next, fn->file);
        }
        /* End of comment. */
        if (state == COMMENT) {
            if (c == '*') {
                next = getc(fn->file);
                if (next == '/')
                    state = NORMAL;
                else
                    ungetc(next, fn->file);
            } else if (c == '\n')
                fn->line++;
            continue;
        }
        /* Start of comment. */
        if (c == '/') {
            next = getc(fn->file);
            if (next == '*') {
                state = COMMENT;
                continue;
            }
            ungetc(next, fn->file);
        }
        /* End of line, return if we have some content. */
        if (c == '\n') {
            fn->line++;
            if (last != '\0')
                break;
            else
                continue;
        }
        /* Count non-whitespace. */
        if (!isblank(c)) {
            last = c;
        }

        /* Make sure we have room for trailing null byte, and copy character. */
        if (i + 1 >= *n) {
            *n = (i + 1) * 2;
            *lineptr = realloc(*lineptr, (*n) * sizeof(**lineptr));
        }
        (*lineptr)[i++] = c;
    }

    (*lineptr)[i] = '\0';
    return i;
}

int getprepline(char **buffer)
{
    extern int VERBOSE;

    static char *line;
    static size_t size;

    int read, processed;

    while (1) {
        if (!src_count) {
            return -1;
        }

        read = getcleanline(&line, &size, &src_stack[src_count - 1]);
        if (read == 0) {
            if (pop() == EOF) {
                return -1;
            }
            continue;
        }

        processed = read;
        break;
    }

    *buffer = line;
    current_file = src_stack[src_count - 1];

    if (VERBOSE) {
        printf("(%s, %d): `%s`\n", current_file.name, current_file.line, line);
    }

    return processed;
}
