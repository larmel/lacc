#include <stdio.h>

extern int bar;

int foo(int n) {
	bar++;
	return printf("%d\n", n);
}
