#include "error.h"
#include "input.h"
#include "util/stack.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Path of initial file, used for relative include paths. */
static const char *directory;

/* List of directories to search on resolving include directives. */
static const char **search_path;
static size_t search_path_count;

/* Keep stack of file descriptors as resolved by includes. Make helper
 * functions for pushing (#include) and popping (EOF) of files, keeping track
 * of the file name and line number for diagnostics. */
static stack_t sources;

typedef struct {
    FILE *file;
    const char *name;
    const char *directory;
    const char *path;
    int line;
} source_t;

static int pop()
{
    source_t *source = (source_t *) stack_pop(&sources);
    if (source != NULL) {
        if (source->file != stdin) {
            fclose(source->file);
        }
        free(source);
        source = (source_t *) stack_peek(&sources);
        if (source != NULL) {
            filename = source->name;
            directory = source->directory;
            fullpath = source->path;
            return 1;
        }
    }
    return EOF;
}

static char *create_path(const char *dir, const char *name)
{
    char *path;

    path = malloc(strlen(dir) + 1 + strlen(name) + 1);
    strcpy(path, dir);
    strcat(path, "/");
    strcat(path, name);

    return path;
}

/* First search current directory, then go through list of search paths. */
static void include_file_internal(const char *name, int incurrent)
{
    source_t *source;
    int i;

    source = calloc(1, sizeof(source_t));
    source->name = strdup(name);

    if (incurrent) {
        source->path = create_path(directory, name);
        source->file = fopen(source->path, "r");
        source->directory = directory;
    }

    if (!source->file) {
        i = search_path_count - 1;
        if (!incurrent) {
            i--; /* skip invocation dir for system files. */
        }
        for (; i >= 0 && !source->file; --i) {
            if (source->path) {
                free((void*) source->path);
                source->path = NULL;
            }
            source->path = create_path(directory, name);
            source->file = fopen(source->path, "r");
            source->directory = directory;
        }
    }

    if (!source->file) {
        error("Unable to resolve include file %s.", name);
        exit(1);
    }

    fullpath = source->path;
    filename = source->name;
    directory = source->directory;
    stack_push(&sources, source);
}

void include_file(const char *name)
{
    include_file_internal(name, 1);
}

void include_system_file(const char *name)
{
    include_file_internal(name, 0);
}

/* Clean up all dynamically allocated resources. */
static void finalize()
{
    while (pop() != EOF)
        ;
    stack_finalize(&sources);
}

void add_include_search_path(const char *path)
{
    /* For the first time, add default search paths at the bottom. */
    if (!search_path) {
        search_path = malloc(3 * sizeof(char *));
        add_include_search_path("/usr/include");
        add_include_search_path("/usr/local/include");
    }

    search_path_count++;
    search_path = realloc(search_path, search_path_count * sizeof(char *));
    search_path[search_path_count - 1] = path;
}

/* Initialize with root file name, and store relative path to resolve later
 * includes. Default to stdin. */
void
init(char *path)
{
    source_t *source;

    source = calloc(1, sizeof(source_t));
    source->file = stdin;
    source->path = "<stdin>";
    source->name = "<stdin>";
    source->directory = ".";
    source->line = 0;

    if (path) {
        char *sep;

        source->name = path;
        sep = strrchr(path, '/');
        if (sep) {
            *sep = '\0';
            source->name = sep + 1;
            source->directory = path;
        }

        source->path = create_path(source->directory, source->name);
        source->file = fopen(source->path, "r");
        if (!source->file) {
            error("Unable to open file %s.", source->path);
            exit(1);
        }
    }

    fullpath = source->path;
    filename = source->name;
    directory = source->directory;
    stack_push(&sources, source);

    add_include_search_path(source->directory);

    atexit(finalize);
}

/* Read characters from stream and assemble a line. Keep track of and remove
 * comments, join lines ending with '\', and ignore all-whitespace lines. Trim
 * leading whitespace, guaranteeing that the first character is '#' for
 * preprocessor directives. Increment line counter in fnt struct for each line
 * consumed. */
static ssize_t
getcleanline(char **lineptr, size_t *n, source_t *fn)
{
    enum { NORMAL, COMMENT } state = 0;
    int c, next; /* getc return values */
    int i = 0, /* chars written to output buffer */
        nonwhitespace = 0; /* non-whitespace characters written */
    FILE *stream;

    assert(fn);

    stream = fn->file;

    /* Need to have room for terminating \0 byte. */
    if (!*n) {
        *n = 1;
        *lineptr = malloc(sizeof(char));
    }

    while ((c = getc(stream)) != EOF) {
        /* line continuation */
        if (c == '\\') {
            next = getc(stream);
            if (next == EOF) {
                error("Invalid end of file after line continuation, aborting");
                exit(0);
            }
            if (next == '\n') {
                fn->line++;
                continue;
            }
            ungetc(next, stream);
        }
        /* end of comment */
        if (state == COMMENT) {
            if (c == '*') {
                next = getc(stream);
                if (next == '/')
                    state = NORMAL;
                else
                    ungetc(next, stream);
            } else if (c == '\n')
                fn->line++;
            continue;
        }
        /* start of comment */
        if (c == '/') {
            next = getc(stream);
            if (next == '*') {
                state = COMMENT;
                continue;
            }
            ungetc(next, stream);
        }
        /* end of line, return if we have some content */
        if (c == '\n') {
            fn->line++;
            if (nonwhitespace > 0)
                break;
        }
        /* skip leading whitspace */
        if (isspace(c)) {
            if (nonwhitespace == 0)
                continue;
        } else
            nonwhitespace++;

        /* make sure we have room for trailing null byte, and copy character */
        if (i + 1 >= *n) {
            *n = (i + 1) * 2;
            *lineptr = realloc(*lineptr, sizeof(char) * *n);
        }
        (*lineptr)[i++] = c;
    }

    (*lineptr)[i] = '\0';
    return i;
}

/* Yield next clean line. */
size_t
getprepline(char **buffer)
{
    extern int VERBOSE;

    static char *line;
    static size_t size;
    source_t *source;
    ssize_t read, processed;

    while (1) {
        source = (source_t *)stack_peek(&sources);
        if (!source) {
            return -1;
        }

        read = getcleanline(&line, &size, source);
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
    line_number = source->line;

    if (VERBOSE) {
        printf("(%s, %d): `%s`\n", filename, (int)line_number, line);
    }

    return processed;
}

/* Store and expose current state. */
size_t line_number;
const char *filename;
const char *fullpath;
