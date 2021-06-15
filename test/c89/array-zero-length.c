int printf(const char *, ...);

int foo[], bar[0];
int foo[0];

struct S {
	char data[0];
	char x, y, z;
} s1;

int test(struct S s) {
	return printf("{%d, %d, %d}\n", s.data[0], s.data[1], s.data[2]);
}

int main(void) {
	s1.x = 3;
	s1.y = 2;
	s1.z = 1;

	test(s1);

	return printf("size: %lu, %lu\n", sizeof(foo), sizeof(struct S));
}
