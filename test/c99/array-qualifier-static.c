int printf(const char *, ...);

int foo(int a[static const 1][2]) {
	int bar(int b[restrict volatile static 2]);
	return bar(a[0]);
}

int bar(int * volatile a) {
	return printf("[%d, %d]\n", a[0], a[1]);
}

static int arr[][2] = {
	{1, 2},
	{3, 4},
	{5, 6}
};

int main(void) {
	return foo(arr);
}
