int printf(const char *, ...);

struct id {
	unsigned char hash[10];
};

struct obj {
	unsigned int a : 4;
	unsigned int b : 4;
	struct id oid;
};

static void foo(struct obj *o) {
	char *arr = (char *) o;
	int i;
	for (i = 0; i < sizeof(*o); ++i) {
		printf("%d ", arr[i]);
	}

	printf(" (%lu)\n", sizeof(*o));
}

static void test1(void) {
	struct obj o = {0x3, 0x4, {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'}};
	foo(&o);
}

struct S {
	int f : 17;
	char b[5];
} s1 = {1, '2', '3'};

static void test2(void) {
	int i;
	for (i = 0; i < sizeof(struct S); ++i) {
		printf("%d ", ((char *) &s1)[i]);
	}

	printf(" (%lu)\n", sizeof(struct S));
}

int main(void) {
	test1();
	test2();
	return 0;
}
