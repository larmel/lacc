int puts(const char *);

inline int foo(int a) {
	/* puts should not be added to ELF symtab if foo is not called, even
	   when foo is called from another inline function not called!
	 */
	return puts("Hello");
}

static inline int bar(void) {
	foo(42);
}

int baz(int a) {
	return a + 1;
}

int main(void) {
	baz(2);
	return 0;
}
