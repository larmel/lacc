#include <stdbool.h>

int printf(const char *, ...);

_Bool f = (bool)127;

struct A {
	_Bool a : 1;
	bool b : 1;
} a1 = {2, 1};

struct B {
	_Bool a : 1;
	unsigned b : 4;
	_Bool c : 1;
} b1 = {12341L};

struct C {
	_Bool a : 1;
	unsigned b : 20;
	bool c : 1;
} c1 = {0, 0, 2.31f};

static void values(_Bool arr[]) {
	printf("arr: %d\n", arr[0]);
	printf("a1: {%d, %d}\n", a1.a, a1.b);
	printf("b1: {%d, %d, %d}\n", b1.a, b1.b, b1.c);
	printf("c1: {%d, %d, %d}\n", c1.a, c1.b, c1.c);
}

static void sizes(_Bool b) {
	printf("sizes: %lu, %lu, %lu, %lu, %lu, %lu, %lu, %lu, %lu\n",
		sizeof(b),
		sizeof(b + 1),
		sizeof(_Bool),
		sizeof(struct {_Bool a;}),
		sizeof(struct {_Bool a : 1;}),
		sizeof(union { bool a : 1; bool b : 1;}),
		sizeof(struct A),
		sizeof(struct B),
		sizeof(struct C));
}

static void branches(void) {
	const _Bool b = true;
	bool c = false;

	if (b && c) {
		printf("1\n");
	}
	if (b == 2) {
		printf("2\n");
	}
	if (!!b) {
		printf("3\n");
	}
}

int main(void) {
	values(&f);
	sizes(f);
	branches();
	return 0;
}
