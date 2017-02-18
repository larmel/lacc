int printf(const char *, ...);

const char *s;

int foo(void) {
	s = __func__;
	return printf("%lu\n", sizeof(__func__));
}

int main(void) {
	const char *t = __func__;
	foo();
	return printf("%s, %s\n", t, s);
}
