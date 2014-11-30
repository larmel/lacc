#include "lcc.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/* expose global state to other components */
size_t line_number;
const char *filename;

static ssize_t preprocess_line(char **, size_t *, size_t);

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
        if (fd_idx >= 0) {
            fd[fd_idx].line = line_number;
        }
        fd_idx++;
        fd[fd_idx].file = file;
        fd[fd_idx].name = name;

        filename = name;
        line_number = 0;
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
            line_number = fd[fd_idx].line;
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

/* yield next preprocessed line */
int
getprepline(char **buffer)
{
    /* reuse buffer for all lines, only freed on termination */
    static size_t length = 1024;
    static char *linebuffer;

    ssize_t read;
    ssize_t processed;

    if (linebuffer == NULL) {
        linebuffer = malloc(length);
    }

    while (1) {
        line_number++;
        read = getline(&linebuffer, &length, fd[fd_idx].file);
        if (read == -1) {
            if (pop() == -1) {
                return -1;
            }
            continue;
        }
        processed = preprocess_line(&linebuffer, &length, (size_t)read);
        if (processed > 0) {
            break;
        }
    }

    *buffer = linebuffer;
    printf("%03d  %s\n", (int)line_number, linebuffer);
    return processed;
}

/* Return number of non-commented characters, replacing commented area with
 * space characters for later strtok. Also removes any trailing newline. */
static size_t
clean(char *line, size_t length)
{
    static int comment; /* flag 1 when in comment */

    int i = 0, n = 0;
    while (i < length) {
        if (!comment && !strncmp("/*", &line[i], 2)) {
            line[i++] = ' '; line[i++] = ' ';
            comment = 1;
        }
        else if (comment && !strncmp("*/", &line[i], 2)) {
            line[i++] = ' '; line[i++] = ' ';
            comment = 0;
        }
        else if (comment || isspace(line[i]))
            line[i++] = ' ';
        else 
            i++, n++;
    }
    return n;
}

static int
is_directive(const char *line, size_t length)
{
    int i = 0;
    while (i < length && isspace(line[i]))
        i++;
    return line[i] == '#';
}

/* Return number of chars in resulting preprocessed line, which is stored
 * in linebuffer (possibly reallocated). Lines that are not part of the
 * translation unit, ex. #define, return 0. Invalid input return -1. */
static ssize_t
preprocess_line(char **linebuffer, size_t *length, size_t read)
{
    static int iffalse; /* flag 1 when in false if block */

    /* erase comments, and ignore lines with only whitespace */
    if (clean(*linebuffer, read) == 0)
        return 0;

    /* detect preprocessing directive */
    if (is_directive(*linebuffer, read)) {
        char *token = strtok(*linebuffer, "# ");

        if (!strcmp("endif", token))
            iffalse = 0;

        /* In false block, no other macro declaration can help */
        if (iffalse) return 0;

        if (!strcmp("include", token)) {
            token = strtok(NULL, " \n");

            printf("Including file '%s'\n", token);

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

        } else if (!strcmp("define", token) && !iffalse) {
            char *symbol = strtok(NULL, " \n");
            char *value = strtok(NULL, " \n");
            sym_define(symbol, value);

        } else if (!strcmp("ifndef", token) && !iffalse) {
            char *symbol = strtok(NULL, " \n");
            iffalse = sym_isdefined(symbol);
        }
        return 0;
    }

    return (iffalse) ? 0 : read;
}
