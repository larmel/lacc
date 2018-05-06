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
	printf("sizeof: (%lu, %lu)\n", sizeof(struct S), sizeof(union U));
	printf("offset: (%lu, %lu)\n",
		offsetof(struct S, a),
		offsetof(struct S, b));

	return 0;
}
