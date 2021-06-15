#include <stdarg.h>

int printf(const char *, ...);

struct point {
	int y[2];
} p;

static void test(struct point *p, ...)
{
	va_list args;
	va_start(args, p);

	p->y[0] = va_arg(args, int);
	p->y[1] = va_arg(args, int);

	va_end(args);
}

int main(void) {
	test(&p, 1, 2);
	return printf("%d, %d\n", p.y[0], p.y[1]);
}
