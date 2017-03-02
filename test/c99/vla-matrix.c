int printf(const char *, ...);

void print(int n, int m, int arr[n][m]) {
	int i, j;
	for (i = 0; i < n; ++i) {
		for (j = 0; j < m; ++j) {
			printf("[%d][%d] = %d\n", i, j, arr[i][j]);
		}
	}

	printf("%lu, %lu\n", sizeof(arr[0]), sizeof(*arr));
}

void matrix(int n, int m) {
	int i, arr[n][m];
	for (i = 0; i < n * m; ++i) {
		arr[i / m][i % m] = i;
	}

	print(n, m, arr);
}

int main(void) {
	matrix(2, 3);
	matrix(31, 87);
	matrix(17, 1);
	return 0;
}
