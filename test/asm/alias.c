#include <assert.h>

void test1(void) {
	int a = 2;
	__asm__  (
		"movl $3, %[t] \n"
		: [t] "=rm" (a) 
		:
		:
	);

	assert(a == 3);
}

int main(void) {
	test1();
	return 0;
}
