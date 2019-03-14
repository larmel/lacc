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

#define call(func, ...) func(__VA_ARGS__)

static int empty(void) { return 42; }

int main(void) {
	int a = call(empty);
	return test(a, 5) + call(test, 3, 0) + call(empty);
}
