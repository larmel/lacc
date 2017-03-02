int printf(const char *, ...);

int foo(int n, float a[][n % 2 ? 5 : 6][n - 2]) {
	return printf("(%lu, %lu, %f, %f)\n",
		sizeof(a[0]),
		sizeof(a[n - 1][n + 1]),
		a[0][0][1],
		a[0][0][4]);
}

int main(void) {
	float p[][2][7] = {3.14, 3.1, 5.1, 7.24, 6.12, 8.12, 9.13};
	return foo(6, p) + foo(7, p);
}
