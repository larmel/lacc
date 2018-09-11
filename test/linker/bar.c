#include <stdlib.h>

extern int foo(int n);

int bar;

int main(int argc, char *argv[]) {
	int n;

	bar = argc;
	while (argc-- > 1) {
		n = atoi(argv[argc]);
		foo(n);
	}

	return bar;
}
