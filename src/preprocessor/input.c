#if _XOPEN_SOURCE < 600
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 600 /* isblank */
#endif
#include "input.h"
#include "strtab.h"
#include <lacc/array.h>
#include <lacc/cli.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* Globally exposed for diagnostics info and default macro values.
 */
struct source current_file;

/* Scratchpad buffer for assembling lines to return. The same buffer is
 * resized if needed, enforcing no specific limit on how long a line can
 * be.
 */
static char *input_line;

/* Temporary buffer used to construct search paths.
 */
static char *path_buffer;

/* List of directories to search on resolving include directives.
 */
static array_of(const char *) search_path_list;

/* Keep stack of file descriptors as resolved by includes. Push and pop
 * from the end of the list.
 */
static array_of(struct source) source_stack;

static struct source push(struct source source)
{
    array_push_back(&source_stack, source);
    return source;
}

static int pop(void)
{
    unsigned len;
    struct source source;

    len = array_len(&source_stack);
    if (len) {
        source = array_pop_back(&source_stack);
        if (source.file != stdin) {
            fclose(source.file);
        }
        if (len - 1) {
            current_file = array_get(&source_stack, len - 2);
            return 1;
        }
    }

    return EOF;
}

static void finalize(void)
{
    while (pop() != EOF)
        ;

    array_clear(&source_stack);
    array_clear(&search_path_list);
    free(path_buffer);
    free(input_line);
}

static char *create_path(const char *path, size_t dirlen, const char *name)
{
    static size_t path_buffer_length;
    size_t len;

    len = dirlen + strlen(name) + 2;
    if (len > path_buffer_length) {
        path_buffer_length = len;
        path_buffer = realloc(path_buffer, path_buffer_length);
    }

    strncpy(path_buffer, path, dirlen);
    path_buffer[dirlen] = '/';
    strcpy(path_buffer + dirlen + 1, name);
    return path_buffer;
}

void include_file(const char *name)
{
    const char *path;
    struct source source = {0};

    /* Construct path by combining current directory and include name,
     * which itself can include folders. */
    if (current_file.dirlen) {
        path = create_path(current_file.path, current_file.dirlen, name);
    } else {
        path = name;
    }

    source.file = fopen(path, "r");
    if (source.file) {
        source.path = str_register(path, strlen(path)).str;
        source.dirlen = strrchr(path, '/') - path;
        current_file = push(source);
    } else {
        include_system_file(name);
    }
}

void include_system_file(const char *name)
{
    struct source source = {0};
    const char *path;
    size_t dirlen;
    int i;

    for (i = 0; i < array_len(&search_path_list); ++i) {
        path = array_get(&search_path_list, i);
        dirlen = strlen(path);
        while (path[dirlen - 1] == '/') {
            dirlen--;
            assert(dirlen);
        }
        path = create_path(path, dirlen, name);
        source.file = fopen(path, "r");
        if (source.file) {
            source.path = str_register(path, strlen(path)).str;
            source.dirlen = strrchr(path, '/') - path;
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
    array_push_back(&search_path_list, path);
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

    /* Make sure file handles are closed on exit, and free string
     * buffers. */
    atexit(finalize);
}

/* Read characters from stream and assemble a line.
 *
 *  - Keep track of and remove comments.
 *  - Join lines ending with '\'.
 *
 * Increment line counter in fnt structure for each line consumed.
 * Ignore all-whitespace lines.
 */
static int getcleanline(char **lineptr, size_t *n, struct source *fn)
{
    enum { NORMAL, COMMENT } state = 0;
    int c, next;        /* Return value of getc. */
    int i = 0,          /* Number of chars written to output buffer. */
        last = '\0';    /* Last non-whitespace character consumed. */
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

        /* Make sure we have room for trailing null byte, and copy
         * character. */
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
    static size_t length;

    struct source *source;
    unsigned len;
    int read;

    do {
        len = array_len(&source_stack);
        if (!len) {
            return -1;
        }
        source = &array_get(&source_stack, len - 1);
        read = getcleanline(&input_line, &length, source);
        if (!read && pop() == EOF) {
            return -1;
        }
    } while (!read);

    *buffer = input_line;
    current_file = *source;

    verbose("(%s, %d): `%s`", current_file.path, current_file.line, input_line);
    return read;
}
