static inline int bar(int);

int foo(void) {
	return bar(42);
}

static inline int bar(int n) {
	return n + 1;
}

int main(void) {
	return foo();
}
