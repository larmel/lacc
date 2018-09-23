void abort(void);

void test1(void) {
	int a = 2;
	__asm__ goto  (
		"jmp %l0 \n"
		:
		:
		:
		: end
	);

	abort();
end:
	;
}

int main(void) {
	test1();
	return 0;
}
