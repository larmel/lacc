#include <stdio.h>
#include <stdarg.h>

extern unsigned line_number;
extern const char *filename;

void error(const char *fmtmsg, ...)
{
    va_list args;
    va_start(args, fmtmsg);
    fprintf(stderr, "(%s, %d) error: ", filename, line_number);
    vfprintf(stderr, fmtmsg, args);
    fprintf(stderr, "\n");
    va_end(args);
}
