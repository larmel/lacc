int printf(const char *, ...);

static int test1(void) {
	int a[] = {5, (int){2}};
	return printf("%d, %d\n", a[0], a[1]);
}

static int test2(float x) {
	struct S {
		char c;
		float fs[3];
	} s1 = (struct S) { 'a', .fs[0] = (float) { 1.4f } };

	return printf("%c, {%f, %f, %f}\n", s1.c, s1.fs[0], s1.fs[1], s1.fs[2]);
}

int main(void) {
	test1();
	test2(3.14f);
	return 0;
}
