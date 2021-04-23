#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "directive.h"
#include "input.h"
#include "strtab.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define FILE_BUFFER_SIZE 4096

struct source {
    FILE *file;

    /*
     * Total capacity of the line buffer is represented by size. The
     * number of characters already handled, a prefix, is 'processed'.
     * Read is the number of valid characters in the buffer. The
     * processed count grows on successive calls towards the read
     * number. When all read characters are processed, or the remaining
     * interval between (processed, read) does not contain a full line,
     * rewind the buffer, or increase if necessary.
     */
    char *buffer;
    size_t size, processed, read;

    /* Full path, or relative to invocation directory. */
    String path;

    /*
     * Number of characters into path occupied by directory, not
     * including the last slash.
     */
    int dirlen;

    /* Current line. */
    int line;
};

/* Temporary buffer used to construct search paths. */
static char *path_buffer;

/* Buffer for reading lines. */
static char *rline;
static size_t rlen;

/* List of directories to search on resolving include directives. */
static array_of(const char *) search_path_list;

/*
 * List of files to include before first source file, specified with
 * -include option.
 */
static array_of(const char *) include_files;

/*
 * Keep stack of file descriptors as resolved by includes. Push and pop
 * from the end of the list.
 */
static array_of(struct source) source_stack;

/* Expose for diagnostics. */
INTERNAL String current_file_path;
INTERNAL int current_file_line;

static void push_file(struct source source)
{
    assert(source.file);
    assert(!str_is_empty(source.path));

    current_file_line = 0;
    current_file_path = source.path;
    source.buffer = malloc(FILE_BUFFER_SIZE);
    source.size = FILE_BUFFER_SIZE;
    array_push_back(&source_stack, source);
}

static int pop_file(void)
{
    int len;
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

INTERNAL void input_finalize(void)
{
    while (pop_file() != EOF)
        ;

    assert(!array_len(&source_stack));
    array_clear(&source_stack);
    array_clear(&search_path_list);
    array_clear(&include_files);
    free(path_buffer);
    free(rline);
}

static size_t path_dirlen(const char *path)
{
    const char *rchr = strrchr(path, '/');
    return (rchr) ? rchr - path : 0;
}

static char *create_path(const char *path, size_t dirlen, const char *name)
{
    static size_t path_buffer_length;
    size_t required, namelen;

    namelen = strlen(name);
    required = dirlen + namelen + 2;
    if (required > path_buffer_length) {
        path_buffer_length = required;
        path_buffer = realloc(path_buffer, path_buffer_length);
    }

    strncpy(path_buffer, path, dirlen);
    path_buffer[dirlen] = '/';
    strncpy(path_buffer + dirlen + 1, name, namelen + 1);
    return path_buffer;
}

INTERNAL void include_file(const char *name)
{
    const char *path;
    struct source *file;
    struct source source = {0};

    /*
     * Construct path by combining current directory and include name,
     * which itself can include folders. Except for root level, where
     * the whole name is already specified.
     */
    assert(array_len(&source_stack));
    file = &array_back(&source_stack);
    if (file->dirlen && name[0] != '/') {
        path = create_path(str_raw(file->path), file->dirlen, name);
    } else {
        path = name;
    }

    source.file = fopen(path, "r");
    if (source.file) {
        source.path = str_c(path);
        source.dirlen = path_dirlen(path);
        push_file(source);
    } else {
        include_system_file(name);
    }
}

INTERNAL void include_system_file(const char *name)
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
            source.path = str_c(path);
            source.dirlen = path_dirlen(path);
            break;
        }
    }

    if (source.file) {
        push_file(source);
    } else {
        error("Unable to resolve include file '%s'.", name);
        exit(1);
    }
}

INTERNAL int add_include_search_path(const char *path)
{
    array_push_back(&search_path_list, path);
    return 0;
}

INTERNAL int add_include_file(const char *path)
{
    array_push_back(&include_files, path);
    return 0;
}

/*
 * Files specified with -include foo are handled as if the first line
 * of the source file contained '#include "foo"', except that it does
 * not search relative to directory of the source file.
 */
static void inject_include_files(void)
{
    int i;
    struct source source = {0};
    const char *path;

    for (i = array_len(&include_files) - 1; i >= 0; --i) {
        path = array_get(&include_files, i);
        source.file = fopen(path, "r");
        if (source.file) {
            source.path = str_c(path);
            source.dirlen = path_dirlen(path);
            push_file(source);
        } else {
            include_system_file(path);
        }
    }
}

INTERNAL void set_input_file(const char *path)
{
    static String sstdin = SHORT_STRING_INIT("<stdin>");

    const char *sep;
    struct source source = {0};

    while (pop_file() != EOF)
        ;

    if (!rline) {
        rlen = FILE_BUFFER_SIZE;
        rline = calloc(rlen, sizeof(*rline));
    } else {
        assert(rlen > 0);
        rline[0] = '\0';
    }

    if (path) {
        sep = strrchr(path, '/');
        source.path = str_c(path);
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
        source.path = sstdin;
    }

    push_file(source);
    inject_include_files();
}

/*
 * Consume input until encountering end of comment. Return number of
 * characters read, or 0 if end of input reached.
 *
 * This must also handle line continuations, which logically happens
 * before replacing comments with whitespace.
 */
static size_t read_comment(const char *line, int *linecount)
{
    char c;
    const char *ptr;

    ptr = line;
    do {
        c = *ptr++;
        if (c == '*') {
            while (*ptr == '\\' && ptr[1] == '\n') {
                *linecount += 1;
                ptr += 2;
            }
            if (*ptr == '/') {
                return ptr + 1 - line;
            }
        } else if (c == '\n') {
            *linecount += 1;
        }
    } while (c != '\0');
    return 0;
}

/*
 * Read single line comment ending at the first newline. Return number
 * of characters read, or 0 if end of input reached.
 */
static size_t read_line_comment(const char *line, int *linecount)
{
    char c;
    const char *ptr;

    ptr = line;
    do {
        c = *ptr++;
        if (c == '\\' && *ptr == '\n') {
            *linecount += 1;
            ptr++;
        } else if (c == '\n') {
            return ptr - line;
        }
    } while (c != '\0');
    return 0;
}

/*
 * Read trigraph character, produced by pattern '??X', where X is the
 * input.
 *
 * Trigraphs are handled in the very first translation step, before
 * splicing escaped newlines.
 */
static char read_trigraph(char c)
{
    switch (c) {
    case '=':  return '#';
    case '(':  return '[';
    case '/':  return '\\';
    case ')':  return ']';
    case '\'': return '^';
    case '<':  return '{';
    case '!':  return '|';
    case '>':  return '}';
    case '-':  return '~';
    default:
        return 0;
    }
}

/*
 * Consume characters forming a quoted string or character literal.
 *
 * Initial preprocessing must consider quoted text because it is allowed
 * to have embedded comments, which should not be replaced by single
 * whitespace.
 *
 * Handle trigraphs and line continuations as in normal input.
 */
static size_t read_literal(const char *line, char **buf, int *lines)
{
    char c;
    char *ptr;
    int count;
    const char *end, q = *line;

    end = line;
    ptr = *buf;
    assert(q == '"' || q == '\'');
    *ptr++ = *end++;

    while ((c = *end) != '\0') {
        switch (c) {
        case '\n':
            if (ptr[-1] == '\\') {
                *lines += 1;
                ptr -= 1;
                end += 1;
                continue;
            } else {
                error("Unexpected newline in literal.");
                exit(1);
            }
            break;
        case '?':
            if (end[1] == '?') {
                c = read_trigraph(end[2]);
                if (c) {
                    end += 3;
                    *ptr++ = c;
                    continue;
                }
            }
            break;
        case '\'':
        case '"':
            if (c == q) {
                count = 0;
                while (ptr[-(count + 1)] == '\\') {
                    count++;
                }
                if (count % 2 == 0) {
                    *ptr++ = *end++;
                    *buf = ptr;
                    return end - line;
                }
            }
        default:
            break;
        }

        *ptr++ = *end++;
    }

    return 0;
}

/*
 * Read initial part of line, until forming a complete source line ready
 * for tokenization. Store the result with the following mutations done:
 *
 *  - Join line continuations.
 *  - Replace comments with a single whitespace character.
 *  - Replace trigraph sequence with corresponding character.
 *
 * Return non-zero number of consumed characters, or 0 if input buffer
 * does not contain a complete line. Note that the source code line can
 * be smaller than this number, by any of the transformations removing
 * characters.
 */
INTERNAL size_t read_line(
    const char *line,
    size_t len,
    char *write,
    int *linecount)
{
    char c;
    int lines;
    size_t count, i, n;
    const char *start;

    assert(write[-1] == '\0');
    count = 0;
    lines = 0;
    start = line;

    for (i = 0; i < len; ++i) {
        switch (line[i]) {
        case '\n':
            if (count) {
                memcpy(write, start, count);
            }
            write[count] = '\0';
            *linecount += lines + 1;
            return i + 1;
        case '"':
        case '\'':
        case '/':
        case '?':
        case '\\':
            break;
        default:
            count++;
            continue;
        }

        break;
    }

    for (; i < len; ++i) {
        switch (line[i]) {
        case '\n':
            if (count) {
                memcpy(write, start, count);
            }
            write[count] = '\0';
            *linecount += lines + 1;
            return i + 1;
        case '"':
        case '\'':
        case '*':
        case '/':
        case '?':
        case '\\':
            if (count) {
                memcpy(write, start, count);
                write += count;
                start += count;
                count = 0;
            }
            c = line[i];
            if (c == '"' || c == '\'') {
                n = read_literal(&line[i], &write, &lines);
                if (!n) {
                    return 0;
                }
                i += n - 1;
                start = &line[i + 1];
            } else if (c == '*' && write[-1] == '/') {
                n = read_comment(&line[i + 1], &lines);
                if (!n) {
                    return 0;
                }
                write[-1] = ' ';
                i += n;
                start = &line[i + 1];
            } else if (c == '\\' && line[i + 1] == '\n') {
                i += 1;
                start = &line[i + 1];
                lines += 1;
            } else if (c == '?' && line[i + 1] == '?') {
                c = read_trigraph(line[i + 2]);
                if (c) {
                    i += 2;
                    start = &line[i + 1];
                    *write++ = c;
                }
            } else if (c == '/' && write[-1] == '/') {
                n = read_line_comment(&line[i + 1], &lines);
                if (!n) {
                    return 0;
                }
                write[-1] = '\0';
                *linecount += lines + 1;
                return i + n + 1;
            } else {
                count++;
            }
            break;
        default:
            count++;
            break;
        }
    }

    return 0;
}

/*
 * Read the next line from file input, doing initial pre-preprocessing.
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
        added = read_line(
            fn->buffer + fn->processed,
            fn->read - fn->processed,
            rline + 1,
            &fn->line);

        if (!added) {
            if (!fn->processed) {
                fn->size += FILE_BUFFER_SIZE;
                fn->buffer = realloc(fn->buffer, fn->size);
                if (rlen <= fn->size) {
                    rlen = fn->size + 1;
                    rline = realloc(rline, rlen);
                }
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
    return rline + 1;
}

static int is_directive(const char *line)
{
    while (*line == ' ' || *line == '\t') {
        line++;
    }

    return *line == '#';
}

INTERNAL char *getprepline(void)
{
    static int stale;

    struct source *source;
    char *line;
    int loc;

    do {
        if (!array_len(&source_stack)) {
            return NULL;
        }
        source = &array_back(&source_stack);
        loc = source->line;
        if (stale) {
            current_file_path = source->path;
            current_file_line = source->line;
            stale = 0;
        }
        line = initial_preprocess_line(source);
        current_file_line += source->line - loc;
        if (!line) {
            stale = 1;
            if (pop_file() == EOF) {
                return NULL;
            }
        }
        if (!in_active_block() && !is_directive(line)) {
            line = NULL;
        }
    } while (!line);

    if (context.verbose) {
        verbose("(%s, %d): `%s`", str_raw(source->path), source->line, line);
    }

    return line;
}
