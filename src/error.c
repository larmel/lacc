#include "error.h"

#include <stdio.h>
#include <stdarg.h>

unsigned errors;

void error(const char *fmtmsg, ...)
{
    va_list args;

    errors++;
    va_start(args, fmtmsg);
    fprintf(stderr, "(%s, %d) error: ", current_file.name, current_file.line);
    vfprintf(stderr, fmtmsg, args);
    fprintf(stderr, "\n");
    va_end(args);
}
