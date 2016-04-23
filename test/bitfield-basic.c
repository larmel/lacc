struct fields {
	signed int foo:7;
	unsigned int bar: (1 + 1);
	unsigned int:0;
	unsigned int baz:4;
};

int main(void) {
	struct fields test = {0};
	struct fields *ref = &test;

	test.foo = 127;
	test.baz = ref->foo;
	ref->bar = 3;
	ref->foo += 1;
	test.bar += 1;

	return test.foo + ref->bar + test.baz;
}
