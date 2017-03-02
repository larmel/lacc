int printf(const char *, ...);

#define M 10

void print(int n, int (*arr)[n]) {
	int i, j;
	for (i = 0; i < M; ++i) {
		for (j = 0; j < n; ++j) {
			printf("[%d][%d] = %d\n", i, j, arr[i][j]);
		}
	}
}

void pointer(int n, int arr[][n]) {
	int (*p)[n], (*q)[n], (*t)[n];

	p = &arr[1];
	q = &arr[M - 3];

	printf("diff: %ld, %ld\n", q - p, q + 1 - p);

	t = q - 5;
	(*t)[4] = 3333;
	t[0][n - 1] = 9127684;

	printf("deref: %d, %d\n", (*t)[1], (*t)[n - 3]);
}

void test(int n) {
	int i, arr[M][n];
	for (i = 0; i < n * M; ++i) {
		arr[i / n][i % n] = i;
	}

	pointer(n, arr);
	print(n, arr);
}

int main(void) {
	test(17);
	test(31);
	test(5);
	return 0;
}
