#include <stdarg.h>

int printf(const char *, ...);

static int foo(int n, ...) {
	int i;
	va_list args, copy;

	va_start(args, n);
	printf("%d\n", va_arg(args, int));
	va_copy(copy, args);
	for (i = 0; i < n - 1; ++i) {
		printf("%d\n", va_arg(copy, int));
	}

	printf("%d\n", va_arg(args, int));
	va_end(args);
	va_end(copy);
	return 0;
}

int main(void) {
	return foo(2, 1, 4)
		+ foo(3, 9, 8, 6)
		+ foo(8, 4, 5, 2, 89, 0, 71, 99, 4);
}
