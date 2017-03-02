int printf(const char *, ...);

int bar(int n) {
	int a[1][n];
	int i = 0, foo(int n, int (*a)[n]);

	for (i = 0; i < n; ++i) {
		a[0][i] = i + 1;
	}

	return foo(n, a);
}

int foo(int n, int a[][n]) {
	while (n--) {
		printf("(*a)[%d] = %d\n", n, a[0][n]);
	}

	return sizeof(*a);
}

int main(void) {
	return bar(16);
}
