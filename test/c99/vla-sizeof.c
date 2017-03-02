int printf(const char *, ...);

int main(void) {
	int n = 42, m = 13;
	return printf("%lu, %lu, %lu, %lu\n",
		sizeof(long double [n + m]),
		sizeof(float *[n]),
		sizeof(long [n][m]),
		sizeof(int [n > 10 ? m + 1 : n]));
}
