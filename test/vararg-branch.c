#include <stdarg.h>
#include <stdio.h>

static void check(const char *start, ...)
{
	const char *key;
	va_list ap;

	va_start(ap, start);
	while ((key = va_arg(ap, const char *))) {
		if (va_arg(ap, int)) {
			printf("%s\n", key);
		} else {
			printf("not %s\n", key);
		}
	}

	va_end(ap);
}

int main(void) {
	check("foo", "a", 1, "b", 0, NULL);
	return 0;
}
