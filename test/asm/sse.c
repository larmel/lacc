#include <assert.h>

void foo(float *f) {
	float p = 0.1f;

	__asm__ volatile (
		"movss (%1), %%xmm1\n"
		"movss %%xmm1, %0\n"
		: "=m" (p)
		: "r" (f)
		: "%xmm1"
	);

	assert(p == *f);
}

void bar(double *d) {
	double p = *d * 4.8 + 0.1;

	__asm__ volatile (
		"movsd %0, %%xmm15\n"
		"movsd %%xmm15, %%xmm8\n"
		"movsd %%xmm8, (%1)\n"
		:
		: "m" (p), "r" (d)
		: "memory", "%xmm8", "%xmm15"
	);

	assert(p == *d);
}

int main(void) {
	float f = 3.14f;
	double d = 2.71;
	foo(&f);
	bar(&d);
	return 0;
}
