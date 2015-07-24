#include <stdarg.h>

int sum(int n, ...)
{
	int value = 0, i;
	va_list args;

	va_start(args, n);
	for (i = 0; i < n; ++i)
		value += va_arg(args, int);

	va_end(args);

	return value;
}

int main() {
	return sum(7, 10, 8, 42, 1, 2, 3, 4);
}
