int printf(const char *, ...);

static void test1(void) {
	int i;
	struct T {
		const int *ptr;
		unsigned int
			foo:1,
			bar:1;
	} foo = { .bar = 1 };

	for (i = 0; i < sizeof(struct T); ++i) {
		printf("%d ", ((char *) &foo)[i]);
	}

	printf(" (%lu)\n", sizeof(struct T));
}

int main(void) {
	test1();
	return 0;
}
