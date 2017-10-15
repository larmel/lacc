int printf(const char *, ...);

static int i = 42;

int main(void) {
	typedef char T;
	T c = 'a';
	printf("i = %d\n", i);
	for (volatile int i = 0, *j; i < 5; ++i)
		printf("%d ", i);

	for (T c;;) {
		c = 'b';
		break;
	}

	printf("c = %d\n", c);
	return 0;
}
