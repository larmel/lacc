#include <stdarg.h>

/* Passed on stack. */
struct object {
	long a, b, c, d;
};

/* Needs two registers. */
struct point {
	char tag;
	int x, y;
};

struct object sum(int n, struct object obj, ...)
{
	int value = 0, i;
	va_list args;

	va_start(args, obj);
	for (i = 0; i < n; ++i) {
		if (i < 3) {
			value += va_arg(args, int);
		} else {
			struct point p = va_arg(args, struct point);
			value += p.x + p.y;
		}
	}
	va_end(args);

	obj.a = value;
	return obj;
}

int main() {
	struct point u = {'u', 2, 3};
	struct point v = {'v', 5, 6};
	struct object obj = {0};
	obj = sum(5, obj, 8, 9, 8, u, v);
	return obj.a + obj.b + obj.c + obj.d;
}
