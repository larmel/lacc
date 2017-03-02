int printf(const char *, ...);

#define N 42

int foo(n, a)
int n;
int a[][n];
{
	int i;
	for (i = 0; i < n; ++i)
		printf("%d: %d\n", i, a[0][i]);
	return n;
}

int main(void) {
	int a[2][N] = {0}, i;
	for (i = 0; i < N; ++i)
		a[0][i] = i + 1;
	return printf("%d\n", foo(N, a));
}
