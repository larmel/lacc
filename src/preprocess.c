#include "error.h"
#include "util/map.h"
#include "util/stack.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

extern int VERBOSE;

/* Expose global state to other components. */
size_t line_number;

const char *filename;

/* Path of initial file, used for relative include paths. */
static const char *directory;

/* Keep stack of file descriptors as resolved by includes. Make helper
 * functions for pushing (#include) and popping (EOF) of files, keeping track
 * of the file name and line number for diagnostics. */
static stack_t sources;

typedef struct {
    FILE *file;
    const char *name;
    const char *directory;
    int line;
} source_t;

static int
pop()
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
            return 1;
        }
    }
    return EOF;
}

static char *
create_path(const char *dir, const char *name)
{
    char *path;

    path = malloc(strlen(dir) + 1 + strlen(name) + 1);
    strcpy(path, dir);
    strcat(path, "/");
    strcat(path, name);

    return path;
}

static void
include_file(const char *name)
{
    source_t *source;
    int i;

    static const char *search_path[] = {
        NULL,
        "/usr/local/include",
        "/usr/include"
    };

    source = calloc(1, sizeof(source_t));
    search_path[0] = directory;
    source->name = strdup(name);

    for (i = 0; i < 3 && !source->file; ++i) {
        char *path;

        path = create_path(search_path[i], name);
        source->directory = search_path[i];
        source->file = fopen(path, "r");

        free(path);
    }

    if (!source->file) {
        error("Unable to resolve include file %s.", name);
        exit(1);
    }

    filename = source->name;
    directory = source->directory;
    stack_push(&sources, source);
}

/* Map between defined symbols and values. Declaring as static handles
 * initialization to empty map. */
static map_t symbols;

/* Keep track of nested #ifndef. */
static stack_t conditions;

/* Clean up all dynamically allocated resources. */
static void
finalize()
{
    while (pop() != EOF)
        ;
    map_finalize(&symbols);
    stack_finalize(&sources);
    stack_finalize(&conditions);
}

/* Initialize with root file name, and store relative path to resolve later
 * includes. Default to stdin. */
void
init(char *path)
{
    source_t *source;

    source = calloc(1, sizeof(source_t));
    source->file = stdin;
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

        path = create_path(source->directory, source->name);
        source->file = fopen(path, "r");
        if (!source->file) {
            error("Unable to open file %s.", path);
            exit(1);
        }
        free(path);
    }

    filename = source->name;
    directory = source->directory;
    stack_push(&sources, source);

    atexit(finalize);
}

static ssize_t getcleanline(char **, size_t *, source_t *);
static ssize_t preprocess_line(char **, size_t);

/* Yield next preprocessed line. */
int
getprepline(char **buffer)
{
    static char *line;
    static size_t size;
    source_t *source;
    ssize_t read, processed;

    while (1) {
        source = (source_t *)stack_peek(&sources);
        read = getcleanline(&line, &size, source);
        if (read == 0) {
            if (pop() == EOF) {
                return -1;
            }
            continue;
        }

        processed = preprocess_line(&line, (size_t)read);
        if (processed > 0) {
            break;
        }
    }

    *buffer = line;
    line_number = source->line;
    if (VERBOSE)
        printf("(%s, %d): `%s`\n", filename, (int)line_number, line);
    return processed;
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
    
    FILE *stream = fn->file;

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

/* Return number of chars in resulting preprocessed line, which is stored
 * in linebuffer (possibly reallocated). Lines that are not part of the
 * translation unit, ex. #define, return 0. Invalid input return -1. */
static ssize_t
preprocess_line(char **linebuffer, size_t read)
{
    static char false = 'f', true = 't';

    if ((*linebuffer)[0] == '#') {
        char *directive = *linebuffer;

        if (!strncmp("#endif", directive, 6)) {
            stack_pop(&conditions);
        } else if (!strncmp("#include", directive, 8)) {
            char *token = strtok(&directive[8], " \n");

            if (strlen(token) > 2 && token[0] == '"' && token[strlen(token)-1] == '"') {
                token[strlen(token)-1] = '\0';
                include_file(token + 1);
                return 0;
            }

            if (strlen(token) > 2 && token[0] == '<' && token[strlen(token)-1] == '>') {
                token[strlen(token)-1] = '\0';
                include_file(token + 1);
                return 0;
            }

            return -1;

        } else if (!strncmp("#define", directive, 7)) {
            char *symbol = strtok(&directive[7], " \n");
            char *value = strtok(NULL, " \n");
            map_insert(&symbols, symbol, (void*) value);

        } else if (!strncmp("#ifndef", directive, 7)) {
            char *symbol = strtok(&directive[7], " \n");
            if (stack_peek(&conditions) == (void *)&false) {
                stack_push(&conditions, (void*) &false);
            } else {
                void *value = (map_lookup(&symbols, symbol) == NULL) 
                    ? (void *)&true : (void *)&false;
                stack_push(&conditions, value);
            }
        }

        return 0;
    }

    return stack_pop(&conditions) == (void *)&false ? 0 : read;
}
