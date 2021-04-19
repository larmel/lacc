#include <assert.h>
#include <stddef.h>
#include <stdint.h>

int printf(const char *, ...);

#define SIZE (1UL << 61)

struct S {
	short buf[SIZE];
	int a;
	int b;
};

union U {
	int a;
	char buf[SIZE];
};

int main(void) {
	assert(sizeof(struct S) == 4611686018427387912);
	assert(sizeof(union U) == 2305843009213693952);
	assert(offsetof(struct S, a) == 4611686018427387904);
	assert(offsetof(struct S, b) == 4611686018427387908);
	return 0;
}
