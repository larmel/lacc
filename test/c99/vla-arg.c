int printf(const char *, ...);

int foo(int n, int a[][n + 1][n]) {
	int b = a[0][0][0];
	int c = a[0][n][n - 1];
	return printf("%d, %d, %lu, %lu\n", b, c, sizeof(a[0]), sizeof(a[1][n - 1]));
}

int main(void) {
	int n = 13;
	int a[2][n][n - 1];

	a[0][0][0] = 42;
	a[0][n - 1][n - 2] = 33;

	return foo(n - 1, a);
}
