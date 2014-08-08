#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

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

/* initialization, called once on root file descriptor */
void
init_preprocessing(FILE *input)
{
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

static ssize_t
preprocess(char **linebuffer, size_t *length, size_t read)
{
    char c;
    int i = 0;
    while (i < read && isspace((*linebuffer)[i]))
        i++;

    /* ignore whitespace line */
    if (i == read) return 0;

    if ((*linebuffer)[i] == '#') {
        /* handle directive */
        return read;
    }

    return read;
}
