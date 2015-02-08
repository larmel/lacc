#ifndef ERROR_H
#define ERROR_H

extern unsigned errors;

/* Error reporting, can be called from any component */
void error(const char *, ...);

#endif
