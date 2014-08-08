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

static ssize_t
preprocess(char **linebuffer, size_t *length, size_t read)
{
    char c;
    char *token;
    int i = 0;
    while (i < read && isspace((*linebuffer)[i]))
        i++;

    /* ignore whitespace line */
    if (i == read) return 0;

    if ((*linebuffer)[i] == '#') {

        /* destructive tokenization of directive */
        token = strtok(&((*linebuffer)[i+1]), " \t");
        if (!strcmp("include", token)) {
            FILE *file;
            char *filename;
            token = strtok(NULL, " \t\n");
            printf("include file: %s\n", token);

            if (strlen(token) > 2 && token[0] == '"' && token[strlen(token)-1] == '"') {
                token[strlen(token)-1] = '\0';
                filename = mkpath(token + 1);
                file = fopen(filename, "r");
                if (file == NULL) {
                    fprintf(stderr, "error: could not open file %s on line %d\n", token, (int)line_number);
                    return -1;
                }
                free(filename);
                push(file);
                line_number = 0;
            } else if (strlen(token) > 2 && token[0] == '<' && token[strlen(token)-1] == '>') {
                return -1;
            } else {
                return -1;
            }

        } else {
            return -1;
        }
        return 0;
    }

    return read;
}
