#include <assert.h>

int foo(int n) {
	return n + 1;
}

int main(void) {
	int (*ptr)(int) = foo;
	int res = 0;

	__asm__ volatile (
		"mov %1, %%rax \n"
		"mov $41, %%edi \n"
		"callq *%%rax \n"
		"mov %%eax, %0 \n"
		: "=rm" (res)
		: "rm" (ptr)
		: "rax", "edi"
	);

	assert(res == 42);
	return 0;
}
