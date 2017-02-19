#include <stdio.h>

/* Example adapted from C11 standard specification. */
#define debug(...) fprintf(stdout, __VA_ARGS__)
#define showlist(...) puts(#__VA_ARGS__)
#define report(test, ...) ((test)?puts(#test): printf(__VA_ARGS__))

int test(int x, int y) {
	debug("Hello World!");
	debug("X = %d\n", x);
	showlist(The first, second, and third items.);
	return report(x>y, "x is %d but y is %d", x, y);
}

int main(void) {
	return test(4, 5) + test(3, 0);
}
