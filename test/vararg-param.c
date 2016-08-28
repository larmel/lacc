#include <stdarg.h>
#include <stdio.h>

static int print(const char *format, va_list ap)
{
	int n = 0, c;

	while ((c = *format++) != '\0') {
		if (c == '#') {
			c = va_arg(ap, int);
		}

		putchar(c);
		n++;
	}

	return n;
}

static int warning(const char *format, ...)
{
	int n;
	va_list args;
	va_start(args, format);
	n = print(format, args);
	putchar('\n');
	va_end(args);
	return n;
}

int main(void) {
	return warning("This # not #", 'A', 'C');
}
