#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int sum(int n, va_list *args) {
	int k = 0;
	while (n--) {
		k += va_arg(*args, double);
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
		test(1, 4.0),
		test(3, 4567.0f, 124.6f, 5368.5f),
		test(4, 123.5, 2356.4, 5678.6, 2769.1));
}
