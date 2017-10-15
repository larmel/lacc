int printf(const char *, ...);

static int * restrict restrict restricted;

int bar(int *restrict * const restrict p) {
	return **p;
}

int foo(int * restrict p) {
	return *p;
}

int main(void) {
	auto int n = 42;
	return foo(&n);
}
