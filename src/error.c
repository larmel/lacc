#include <stdio.h>
#include <stdarg.h>

unsigned errors;

extern size_t line_number;
extern const char *filename;

void error(const char *fmtmsg, ...)
{
    va_list args;

    errors++;
    va_start(args, fmtmsg);
    fprintf(stderr, "(%s, %ld) error: ", filename, line_number);
    vfprintf(stderr, fmtmsg, args);
    fprintf(stderr, "\n");
    va_end(args);
}
