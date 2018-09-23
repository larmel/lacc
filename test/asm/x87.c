int printf(const char *, ...);

void foo(float *f) {
	__asm__ volatile (
		"flds (%0) \n"
		"fistpl (%0) \n"
		"fildl (%0) \n"
		"fstps (%0) \n"
		:
		: "r" (f)
		: "memory"
	);

	return;
}

int main(void) {
	float f = 3.14f;
	float p = f;
	foo(&f);
	printf("%f, %f\n", f, p);
	return 0;
}
