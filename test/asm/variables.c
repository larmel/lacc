int printf(const char *, ...);

void foo(void) {
	int a = 1, b = 2, c = 3, d = 4, e = 5, f = 6;

	a = b + c * d;
	b = (c << 4) + f;
	c = e - 2 ^ b;
	d = 4 * a - c + c;

	__asm__ (
		"mov %2, %%r13d \n\t"
		"add %%r13d, %0 \n\t"
		"add %1, %0 \n\t"
		: "+r" (d)
		: "r" (e), "r" (f)
		: "%r13"
	);

	printf("%d, %d, %d, %d, %d, %d\n", a, b, c, d, e, f);

	__asm__ (
		"add %1, %0 \n\t"
		: "+r" (a)
		: "r" (b)
	);

	printf("%d, %d, %d, %d, %d, %d\n", a, b, c, d, e, f);
}

int main(void) {
	foo();
	return 0;
}
