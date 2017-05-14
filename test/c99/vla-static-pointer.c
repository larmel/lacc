int printf(const char *, ...);

int B[] = {1, 2, 3, 4, 5, 6};

int foo(int n) {
	int i;
	static int (*p)[n] = &B;
	for (i = 0; i < n; ++i) {
		(*p)[i] = n + i;
	}
	return n;
}

int main(void) {
	int i;
	foo(4);
	for (i = 0; i < sizeof(B) / sizeof(int); ++i) {
		printf("%d\n", B[i]);
	}
	return 0;
}
