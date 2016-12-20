#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int sum(int n, va_list *args) {
	int k = 0;
	while (n--) {
		k += va_arg(*args, int);
	}

	return k;
}

static int test(int n, ...) {
	va_list *args;
	args = malloc(sizeof(*args));
	va_start(*args, n);
	n = sum(n, args);
	va_end(*args);
	return n;
}

int main(void) {
	return printf("%d, %d, %d\n",
		test(1, 4),
		test(3, 4567, 124, 5368),
		test(4, 123, 2356, 5678, 2769));
}
