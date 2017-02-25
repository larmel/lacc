#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

struct S1 {
	int n;
	double d[];
};

struct S1 *alloc(int n) {
	struct S1 *p = malloc(sizeof(struct S1) + sizeof(double) * n);
	p->n = n;
	return p;
}

int main(void) {
	struct S1 *s = alloc(3);

	s->d[0] = 41.4;
	s->d[1] = 3.14;
	s->d[2] = 2.71;

	return printf("%lu, %ld, (%f, %f, %f)\n",
		sizeof(struct S1),
		offsetof(struct S1, d),
		s->d[0], s->d[1], (*s).d[2]);
}
