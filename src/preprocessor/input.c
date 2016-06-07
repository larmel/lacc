#if _XOPEN_SOURCE < 600
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 600 /* isblank */
#endif
#include "directive.h"
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

#define FILE_BUFFER_SIZE 4096

struct source {
    FILE *file;

    /* Total capacity of the line buffer is represented by size. The
     * number of characters already handled, a prefix, is 'processed'.
     * Read is the number of valid characters in the buffer. The
     * processed count grows on successive calls towards the read
     * number. When all read characters are processed, or the remaining
     * interval between (processed, read) does not contain a full line,
     * rewind the buffer, or increase if necessary. */
    char *buffer;
    size_t size, processed, read;

    /* Full path, or relative to invocation directory. */
    String path;

    /* Number of characters into path occupied by directory, not
     * including the last slash. */
    int dirlen;

    /* Current line. */
    int line;
};

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

/* Expose for diagnostics.
 */
String current_file_path;
int current_file_line;

static struct source *current_file(void)
{
    assert(array_len(&source_stack));
    return &array_get(&source_stack, array_len(&source_stack) - 1);
}

static void push(struct source source)
{
    assert(source.file);
    assert(source.path.len);

    current_file_line = 0;
    current_file_path = source.path;
    source.buffer = malloc(FILE_BUFFER_SIZE);
    source.size = FILE_BUFFER_SIZE;
    array_push_back(&source_stack, source);
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
        free(source.buffer);
        if (len - 1) {
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
    struct source *file;
    struct source source = {0};

    /* Construct path by combining current directory and include name,
     * which itself can include folders. */
    file = current_file();
    if (file->dirlen) {
        path = create_path(str_raw(file->path), file->dirlen, name);
    } else {
        path = name;
    }

    source.file = fopen(path, "r");
    if (source.file) {
        source.path = str_register(path, strlen(path));
        source.dirlen = strrchr(path, '/') - path;
        push(source);
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
            source.path = str_register(path, strlen(path));
            source.dirlen = strrchr(path, '/') - path;
            break;
        }
    }

    if (source.file) {
        push(source);
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
    const char *sep;
    struct source source = {0};

    if (path) {
        sep = strrchr(path, '/');
        source.path = str_init(path);
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
        source.path = str_init("<stdin>");
    }

    push(source);
    atexit(finalize);
}

static size_t process_chunk(char *line, size_t len, int *linecount)
{
    /* Preserve progress made between resizing or moving. Both for
     * optimization, and to not have to backtrack mutations done on the
     * string, like replacing comments with a single space character. */
    static ptrdiff_t prev_ptr, prev_tail, prev_end;
    static int continuations;
    static enum {
        NORMAL,
        COMMENT
    } state = NORMAL;

    /* Invariant: ptr always points to one past the last character of
     * the result string, at loop exit this should be '\0'. The end
     * pointer always points to, at the start of each iteration, the
     * next character to consider part of the string.
     * Tail is the previous content character skipped over by end. This
     * Needs to be tracked for comments, like *\ (newline) /. Logically,
     * the character at *(end - 1), when skipping continuations.
     * Characters are copied from *end to *ptr as processing moves
     * along, skipping over line continuations and comments.
     * The preprocessing is successful iff *ptr == '\0' at loop exit,
     * meaning we were able to process a whole line. */
    char
        *ptr = line + prev_ptr,
        *end = line + prev_end,
        *tail = line + prev_tail;

    while (end - line < len) {
        assert(*end != '\0');
        if (*end == '\\') {
            if (*(end + 1) == '\0') break;
            else if (*(end + 1) == '\n') {
                continuations++;
                end += 2;
            } else {
                if (ptr != end) {
                    *ptr = *end;
                }
                tail = end++;
                ptr++;
            }
        } else if (*end == '\n') {
            end++;
            if (state == NORMAL) {
                *ptr = '\0';
                (*linecount) += continuations + 1;
                prev_ptr = prev_tail = prev_end = 0;
                continuations = 0;
                return end - line;
            }
        } else {
            if (ptr > line && *(ptr - 1) == '/' && *end == '*' &&
                state == NORMAL)
            {
                state = COMMENT;
                *(ptr - 1) = ' ';
                end++;
            } else if (*tail == '*' && *end == '/' && state == COMMENT) {
                state = NORMAL;
                tail = end++;
            } else if (state == NORMAL) {
                if (ptr != end) {
                    *ptr = *end;
                }
                tail = end++;
                ptr++;
            } else {
                tail = end++;
            }
        }
        assert(end - line <= len);
    }

    prev_ptr = ptr - line;
    prev_end = end - line;
    prev_tail = tail - line;
    return 0;
}

/* Read the next line from file input, doing initial pre-preprocessing.
 */
static char *initial_preprocess_line(struct source *fn)
{
    size_t added;
    assert(fn->buffer);
    assert(fn->processed <= fn->read);
    assert(fn->read < fn->size);

    do {
        if (fn->processed == fn->read || !fn->processed) {
            if (feof(fn->file)) {
                if (fn->read > fn->processed) {
                    error("Unable to process the whole input.");
                    exit(1);
                }
                return NULL;
            }
            if (!fn->processed) {
                fn->read += fread(
                    fn->buffer + fn->read,
                    sizeof(char),
                    fn->size - fn->read - 1,
                    fn->file);
            } else {
                fn->read = fread(
                    fn->buffer,
                    sizeof(char),
                    fn->size - 1,
                    fn->file);
            }
            fn->processed = 0;
            fn->buffer[fn->read] = '\0';
            if (feof(fn->file)) {
                if (!fn->read) {
                    return NULL;
                }
                if (fn->buffer[fn->read - 1] != '\n') {
                    error("Missing newline at end of file.");
                    fn->buffer[fn->read] = '\n';
                }
            }
        }

        assert(fn->processed < fn->read);
        added = process_chunk(
            fn->buffer + fn->processed,
            fn->read - fn->processed,
            &fn->line);

        if (!added) {
            if (!fn->processed) {
                fn->size += FILE_BUFFER_SIZE;
                fn->buffer = realloc(fn->buffer, fn->size);
            } else {
                memmove(
                    fn->buffer,
                    fn->buffer + fn->processed,
                    fn->read - fn->processed);
                assert(fn->read > fn->processed);
                fn->read -= fn->processed;
                fn->processed = 0;
            }
        }
    } while (!added);

    fn->processed += added;
    return fn->buffer + fn->processed - added;
}

static int is_directive(const char *line)
{
    while (isblank(*line))
        line++;
    return *line == '#';
}

char *getprepline(void)
{
    struct source *source;
    unsigned len;
    char *line;

    do {
        len = array_len(&source_stack);
        if (!len) {
            return NULL;
        }
        source = &array_get(&source_stack, len - 1);
        line = initial_preprocess_line(source);
        if (!line && pop() == EOF) {
            return NULL;
        }
        if (!in_active_block() && !is_directive(line)) {
            line = NULL;
        }
    } while (!line);

    current_file_path = source->path;
    current_file_line = source->line;
    verbose("(%s, %d): `%s`", str_raw(source->path), source->line, line);
    return line;
}
