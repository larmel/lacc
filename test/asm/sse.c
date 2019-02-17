#include <assert.h>

int printf(const char *, ...);

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

void sseround(float vec[3]) {
	static unsigned char mask[16] = {
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF,
		0x00, 0x00, 0x00, 0x00
	};

	__asm__ volatile (
		"movaps (%0), %%xmm1 \n"
		"movups (%1), %%xmm0 \n"
		"movaps %%xmm0, %%xmm2 \n"
		"andps %%xmm1, %%xmm0 \n"
		"andnps %%xmm2, %%xmm1 \n"
		"cvtps2dq %%xmm0, %%xmm0 \n"
		"cvtdq2ps %%xmm0, %%xmm0 \n"
		"orps %%xmm1, %%xmm0 \n"
		"movups %%xmm0, (%1) \n"
		:
		: "r" (mask), "r" (vec)
		: "memory", "%xmm0", "%xmm1", "%xmm2"
	);
}

int main(void) {
	float f = 3.14f;
	double d = 2.71;
	float vec[] = {3.14f, 5.66f, 9.61f};

	foo(&f);
	bar(&d);
	sseround(vec);

	printf("%f, %f, %f\n", vec[0], vec[1], vec[2]);
	return 0;
}
