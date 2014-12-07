#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/* expose global state to other components */
size_t line_number;
const char *filename;

/* stack of file descriptors as resolved by includes */
static struct fnt {
    FILE *file;
    const char *name;
    int line;
} *fd = NULL;

static int fd_idx = -1;
static int fd_len;

static int push(const char* name)
{
    FILE *file = fopen(name, "r");
    if (file != NULL) {
        if (fd_idx == fd_len - 1) {
            fd_len += 16;
            fd = realloc(fd, fd_len * sizeof(struct fnt));
        }
        fd_idx++;
        fd[fd_idx].file = file;
        fd[fd_idx].name = name;

        filename = name;
    } else {
        error("could not open file %s", name);
    }
    return fd_idx;
}

static int pop()
{
    if (fd_idx >= 0) {
        fclose(fd[fd_idx].file);
        fd_idx--;
        if (fd_idx >= 0) {
            filename = fd[fd_idx].name;
        }
    }
    return fd_idx;
}

/* path of initial file, used for relative include paths */
static const char *directory;

static char *
mkpath(const char *filename)
{
    size_t dir_len = strlen(directory);
    size_t fil_len = strlen(filename);
    char *path = malloc(dir_len + 1 + fil_len + 1);
    strcpy(path, directory);
    strcat(path, "/");
    strcat(path, filename);
    return path;
}

/* symbol list */
static const char ** symbols;
static const char ** values;
static size_t sym_idx;
static size_t sym_cap;

/*
static const char *
sym_lookup(const char * symbol)
{
    int i;
    for (i = 0; i < sym_idx; ++i) {
        if (!strcmp(symbol, symbols[i])) return values[i];
    }
    return NULL;
}
*/

static int
sym_isdefined(const char * symbol)
{
    int i;
    for (i = 0; i < sym_idx; ++i) {
        if (!strcmp(symbol, symbols[i])) return 1;
    }
    return 0;
}

static int
sym_define(const char *symbol, const char *value) {
    if (sym_idx == sym_cap) {
        sym_cap += 64;
        symbols = realloc(symbols, sym_cap * sizeof(char*));
        values = realloc(values, sym_cap * sizeof(char*));
    }
    symbols[sym_idx] = strdup(symbol);
    values[sym_idx] = value == NULL ? "" : strdup(value);
    return sym_idx++;
}

/* initialization, called once with root file name */
void
preprocess(const char *filename)
{
    char *dir = ".";
    char *lastsep = strrchr(filename, '/');
    if (lastsep != NULL) {
        dir = calloc(lastsep - filename + 1, sizeof(char));
        strncpy(dir, filename, lastsep - filename);
    }

    directory = dir;
    push(filename);
}

static ssize_t getcleanline(char **, size_t *, struct fnt *);
static ssize_t preprocess_line(char **, size_t);

/* Yield next preprocessed line */
int
getprepline(char **buffer)
{
    static char *line;
    static size_t size;
    ssize_t read, processed;

    while (1) {
        read = getcleanline(&line, &size, &fd[fd_idx]);
        if (read == 0) {
            if (pop() == -1) {
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
    line_number = fd[fd_idx].line;
    printf("(%s, %d):  %s\n", filename, (int)line_number, line);
    return processed;
}

/* Read characters from stream and assemble a line. Keep track of and remove
 * comments, join lines ending with '\', and ignore all-whitespace lines. Trim
 * leading whitespace, guaranteeing that the first character is '#' for
 * preprocessor directives. Increment line counter in fnt struct for each line
 * consumed. */
static ssize_t
getcleanline(char **lineptr, size_t *n, struct fnt *fn)
{
    enum { NORMAL, COMMENT } state = 0;
    int c, next; /* getc return values */
    int i = 0, /* chars written to output buffer */
        nonwhitespace = 0; /* non-whitespace characters written */

    FILE *stream = fn->file;

    while ((c = getc(stream)) != EOF) {
        /* line continuation */
        if (c == '\\') {
            next = getc(stream);
            if (next == EOF) {
                error("Invalid end of file after line continuation, aborting");
                exit(0);
            }
            if (next == '\n')
                fn->line++;
            else
                ungetc(next, stream);
            continue;
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
    static int iffalse; /* flag 1 when in false if block */

    /* detect preprocessing directive */
    if ((*linebuffer)[0] == '#') {
        char *directive = *linebuffer;

        /* Endif needs to be considered iff in false block */
        if (iffalse) {
            if (!strncmp("#endif", directive, 6)) {
                iffalse = 0;
            }
        } else if (!strncmp("#include", directive, 8)) {
            char *token = strtok(&directive[8], " \n");

            if (strlen(token) > 2 && 
                token[0] == '"' && 
                token[strlen(token)-1] == '"')
            {
                token[strlen(token)-1] = '\0';
                push(mkpath(token + 1));
            } else if (strlen(token) > 2 && 
                token[0] == '<' && 
                token[strlen(token)-1] == '>') {
                return 0;
            } else {
                return -1;
            }

        } else if (!strncmp("#define", directive, 7)) {
            char *symbol = strtok(&directive[7], " \n");
            char *value = strtok(NULL, " \n");
            sym_define(symbol, value);

        } else if (!strncmp("#ifndef", directive, 7)) {
            char *symbol = strtok(&directive[7], " \n");
            iffalse = sym_isdefined(symbol);
        }
        return 0;
    }

    return (iffalse) ? 0 : read;
}
