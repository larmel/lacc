int printf(const char *, ...);

union U1 {
	struct {
		char c;
		char s[];
	} foo;
	int a;
};

union U2 {
	int b;
	union U1 bar;
};

union U1 test(void) {
	union U1 p = {3};
	p.foo.s[0] = 1;
	p.foo.s[1] = 2;
	p.foo.s[2] = 3;
	return p;
}

int main(void) {
	union U1 p = test();
	union U2 q = {0};

	q.bar = p;
	return printf("%lu, %lu (%d, %d, %d)\n",
		sizeof(union U1),
		sizeof(union U2),
		q.bar.foo.s[0], q.bar.foo.s[1], q.bar.foo.s[2]);
}
