int printf(const char *, ...);

int *foo(int n) {
	int (*a)[n];
	int (* volatile b)[n + 1];
	static int k[] = {1, 2, 3};

	a = &k;
	b = a;
	return *a;
}

int main(void) {
	return printf("%d, %d\n", *foo(3), foo(5)[2]);
}
