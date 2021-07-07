#include <assert.h>

void test1(int a) {
	int b = 0;
	__asm__ (
		"mov $5, %%eax \n"
		"mov %%eax, %0 \n /* Ignore comment. */"
		"mov $3, %%ebx \n"
		"add %%ebx, %1 \n"
		: "=m" (a), "=m" (b)
		:
	);

	assert(a == 5);
	assert(b == 3);
}

void test2(int a) {
	int *p = &a;
	int b = 2;
	__asm (
		"movl $5, %0 \n"
		"movl $6, %1 \n"
		: "=m" (*p), "=m" (b)
		:
	);

	assert(a == 5);
	assert(b == 6);
}

void test3(void) {
	int a = 1;
	int b = 2;

	__asm__ (
		"add $2, %1 \n\t"
		"mov %1, %0 \n\t"
		: "=r" (a)
		: "r" (b)
	);

	assert(b == 2);
	assert(a == 4);

	__asm__ (
		"add $2, %1 \n\t"
		"mov %1, %0 \n\t"
		: "=r" (a), "+r" (b)
		:
	);

	assert(b == 4);
	assert(a == 4);
}

void test4(void) {
	int a[] = {1, 2, 3, 4};
	int b = 0;
	__asm__ (
		"leaq %1, %%r10 \n\t"
		"mov %2, %%r15d \n\t"
		"leaq (, %%r15, 4), %%rax \n\t"
		"mov (%%r10, %%rax, 1), %0 \n\t"
		: "=r" (b)
		: "m" (a[0]), "r" (a[1])
		: "rax", "r10", "r15"
	);

	assert(b == a[2]);
}

void test5(void) {
	int a = 1;
	__asm__ (
		"add $1, %0 \n"
		: "+rm" (a)
	);

	assert(a == 2);

	__asm__ (
		"add $1, %0 \n"
		: "+rm" (a)
		:
	);

	assert(a == 3);
}

void test6(void) {
	static unsigned char data[4] = { "\xFF\xFF\xFF\xFF" };

	unsigned int a = 0;
	__asm__ volatile (
		"mov (%1), %0 \n"
		: "=r" (a)
		: "r" (data)
	);

	assert(a == 0xffffffff);
}

int main(void) {
	test1(42);
	test2(42);
	test3();
	test4();
	test5();
	test6();
	return 0;
}
