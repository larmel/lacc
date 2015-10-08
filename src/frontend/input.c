#if _XOPEN_SOURCE < 600
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup */
#endif
#include "core/error.h"
#include "core/string.h"
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

/* Keep stack of file descriptors as resolved by includes. Push and pop from
 * the end of the list.
 */
static struct source *src_stack;
static size_t src_count;

static struct source push(struct source source)
{
    src_count++;
    src_stack = realloc(src_stack, src_count * sizeof(*src_stack));
    src_stack[src_count - 1] = source;
    return source;
}

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

static void finalize(void)
{
    assert(src_stack);

    while (pop() != EOF)
        ;

    free(src_stack);
    if (search_path)
        free(search_path);
}

void include_file(const char *name)
{
    struct source source = {0};

    /* Construct path by combining current directory and include name, which
     * itself can include folders. */
    if (current_file.dirlen) {
        int length = current_file.dirlen + strlen(name) + 1;
        char *path = calloc(length + 1, sizeof(*path));

        strncpy(path, current_file.path, current_file.dirlen);
        path[current_file.dirlen] = '/';
        strcpy(path + current_file.dirlen + 1, name);
        source.path = str_register_n(path, length);
        free(path);

        path = strrchr(source.path, '/');
        source.dirlen = path - source.path;
    } else {
        source.path = name;
    }

    source.file = fopen(source.path, "r");
    if (source.file) {
        current_file = push(source);
    } else {
        include_system_file(name);
    }
}

void include_system_file(const char *name)
{
    /* Re-use static buffer to save some allocations. Each lookup constructs
     * new strings by combining include directory and filename. There is no
     * specific limit on the length of file names. */
    static char *path;
    static size_t length;

    struct source source = {0};
    int i;

    assert(search_path_count);

    for (i = 0; i < search_path_count; ++i) {
        size_t dir = strlen(search_path[i]);
        size_t len = dir + strlen(name) + 1;

        if (len > length) {
            length = len * 2;
            path = realloc(path, length * sizeof(*path));
        }

        strcpy(path, search_path[i]);
        if (path[dir - 1] == '/') {
            /* Include paths can be specified with or without trailing slash.
             * Do not normalize initially, but handle it here. */
            dir--;
        } else {
            path[dir] = '/';
        }

        strcpy(path + dir + 1, name);
        source.file = fopen(path, "r");
        if (source.file) {
            char *end = strrchr(path, '/');
            source.path = str_register_n(path, len);
            source.dirlen = end - path;
            break;
        }
    }

    if (source.file) {
        current_file = push(source);
    } else {
        error("Unable to resolve include file '%s'.", name);
        exit(1);
    }
}

void add_include_search_path(const char *path)
{
    static size_t cap;

    if (!cap) {
        cap = 16;
        search_path = calloc(cap, sizeof(*search_path));
    } else if (search_path_count + 1 == cap) {
        cap *= 2;
        search_path = realloc(search_path, cap * sizeof(*search_path));
    }

    assert(search_path_count < cap);
    search_path_count++;
    search_path[search_path_count - 1] = path;
}

void init(const char *path)
{
    struct source source = {0};

    if (path) {
        const char *sep = strrchr(path, '/');
        source.path = path;
        source.file = fopen(path, "r");
        if (sep) {
            source.dirlen = sep - path;
        }
        if (!source.file) {
            error("Unable to open file %s.", path);
            exit(1);
        }
    } else {
        source.file = stdin;
        source.path = "<stdin>";
    }

    current_file = push(source);

    /* Make sure file handles are closed on exit. */
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
        printf("(%s, %d): `%s`\n", current_file.path, current_file.line, line);
    }

    return processed;
}
