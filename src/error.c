#include <stdio.h>
#include <stdarg.h>

extern unsigned line_number;
extern const char *filename;

unsigned errors;

void error(const char *fmtmsg, ...)
{
    va_list args;

    errors++;
    va_start(args, fmtmsg);
    fprintf(stderr, "(%s, %d) error: ", filename, line_number);
    vfprintf(stderr, fmtmsg, args);
    fprintf(stderr, "\n");
    va_end(args);
}
