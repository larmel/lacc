#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

static ssize_t preprocess(char **, size_t *, size_t);

/* stack of file descriptors as resolved by includes */
static FILE* *fd = NULL;

/* stack functionality */
static size_t fd_idx = -1;
static size_t fd_len;

static size_t push(FILE *file)
{
    int i = 0;
    if (fd_idx == fd_len - 1) {
        fd_len += 16;
        fd = realloc(fd, fd_len * sizeof(FILE*));
    }
    fd[++fd_idx] = file;
    return fd_idx;
}

static size_t pop()
{
    if (fd_idx == 0) {
        free(fd);
        fd_len = 0;
    }
    return --fd_idx;
}

/* symbol list */
static const char ** symbols;
static const char ** values;
static size_t sym_idx;
static size_t sym_cap;

static const char *
sym_lookup(const char * symbol)
{
    int i;
    for (i = 0; i < sym_idx; ++i) {
        if (!strcmp(symbol, symbols[i])) return values[i];
    }
    return NULL;
}

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

/* path of initial file, used for relative include paths */
static const char *directory;

/* initialization, called once on root file descriptor */
void
init_preprocessing(FILE *input, const char *dir)
{
    directory = dir;
    while (fd_idx != -1)
        pop();

    push(input);
}

/* keep track of line number within a file */
size_t line_number;

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
        read = getline(&linebuffer, &length, fd[fd_idx]);
        if (read == -1) {
            if (pop() == -1) {
                return -1;
            }
            line_number = 0;
            continue;
        }
        processed = preprocess(&linebuffer, &length, (size_t)read);
        if (processed > 0) {
            break;
        }
    }

    *buffer = linebuffer;
    return processed;
}

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

/* Return number of chars in resulting preprocessed line, which is stored
   in linebuffer (possibly reallocated). Lines that are not part of the
   translation unit, ex. #define, return 0. Invalid input return -1. */
static ssize_t
preprocess(char **linebuffer, size_t *length, size_t read)
{
    char c;
    char *token;
    int i = 0;
    while (i < read && isspace((*linebuffer)[i]))
        i++;

    /* flag 1 when in false if block */
    static int grayzone;

    /* ignore whitespace line */
    if (i == read) return 0;

    if ((*linebuffer)[i] == '#') {

        /* destructive tokenization of directive */
        token = strtok(&((*linebuffer)[i+1]), " \t");
        if (!strcmp("include", token) && !grayzone) {
            FILE *file;
            char *filename;
            token = strtok(NULL, " \t\n");

            if (strlen(token) > 2 && 
                token[0] == '"' && 
                token[strlen(token)-1] == '"')
            {
                token[strlen(token)-1] = '\0';
                filename = mkpath(token + 1);
                file = fopen(filename, "r");
                if (file == NULL) {
                    fprintf(stderr, 
                        "error: could not open file %s on line %d\n", 
                        token, (int)line_number);
                    return -1;
                }
                free(filename);
                push(file);
                line_number = 0;
            } else if (strlen(token) > 2 && 
                token[0] == '<' && 
                token[strlen(token)-1] == '>') {
                return 0;
            } else {
                return -1;
            }

        } else if (!strcmp("define", token) && !grayzone) {
            char *symbol = strtok(NULL, " \n\t");
            char *value = strtok(NULL, " \n\t");
            sym_define(symbol, value);

        } else if (!strcmp("ifndef", token) && !grayzone) {
            char *symbol = strtok(NULL, " \n\t");
            grayzone = sym_isdefined(symbol);

        } else if (!strcmp("endif", token)) {
            grayzone = 0;
        }

        return 0;
    }

    return (grayzone) ? 0 : read;
}
