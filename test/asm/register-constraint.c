int printf(const char *, ...);

static int foo(int a, int b, int c, int d) {
	int e;

	__asm__ volatile (
		"mov %%ebx, %0 \n"
		: "=rm" (e)
	);

	return printf("%d, %d, %d, %d, %d\n", a, b, c, d, e);
}

int main(void) {
	int a = 3, b = 5, c = 9, d = 7, e = 2;
	int (*ptr)(int, int, int, int) = foo;

	__asm__ volatile (
		"call *%%rax \n"
		"mov $11, %%edi \n"
		"mov $12, %%esi \n"
		"mov $13, %%ebx \n"
		"mov $0, %%eax \n"
		"mov $15, %%ecx \n"
		"mov $16, %%edx \n"
		: "+S" (a), "+D" (b), "+b" (c)
		: "a" (ptr), "c" (d), "d" (e)
		: "cc", "memory"
	);

	return printf("%d, %d, %d, %d, %d, (%d)\n", a, b, c, d, e, ptr == foo);
}
