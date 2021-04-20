#include <assert.h>

long double
	x = 3.14,
	y = 145260912182745.12486L,
	z = -972316.70L;

int main(void) {
	char a;
	unsigned char b;
	int c;
	unsigned int d;
	long e;
	unsigned long f;
	float g;
	double h;

	(a = x), assert(a == 3);
	(b = x), assert(b == 3);
	(c = x), assert(c == 3);
	(d = x), assert(d == 3);
	(e = x), assert(e == 3);
	(f = x), assert(f == 3);
	(g = x), assert(g == 3.14f);
	(h = x), assert(h == 3.14);

	(a = y), assert(a == 0);
	(b = y), assert(b == 0);
	(c = y), assert(c == -2147483648);
	(d = y), assert(d == 823264729);
	(e = y), assert(e == 145260912182745);
	(f = y), assert(f == 145260912182745);
	(g = y), assert(g == 145260911001600.000000f);
	(h = y), assert(h == 145260912182745.125000);

	(a = z), assert(a == 0);
	(b = z), assert(b == 0);
	(c = z), assert(c == -972316);
	(d = z), assert(d == 4293994980);
	(e = z), assert(e == -972316);
	(f = z), assert(f == 18446744073708579300ul);
	(g = z), assert(g == -972316.687500f);
	(h = z), assert(h == -972316.700000);

	return 0;
}
