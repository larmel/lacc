int printf(const char *, ...);

void print(unsigned n, int arr[n]) {
	int i;
	for (i = 0; i < n; ++i) {
		printf("%d\n", arr[i]);
	}
}

int foo(unsigned short n) {
	int i, a[n];
	int b[n];

	for (i = 0; i < n; ++i) {
		a[i] = i;
		b[i] = a[i] - i;
	}

	print(n, a);
	print(n, b);

	for (i = 0; i < n; ++i) {
		a[i] = a[i] + b[i];
	}

	print(n, a);
	return printf("%lu\n", sizeof(a));
}

int main(void) {
	return foo(8);
}
