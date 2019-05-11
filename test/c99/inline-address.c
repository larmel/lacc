int puts(const char *);

static inline int foo(int a) {
	return puts("Hello");
}

static inline int bar(void) {
	foo(42);
}

/* Declaring a pointer has same effect as calling, now inlines are defined. */
int (*f)(void) = &bar;

static inline int baz(int a) {
	return a + 1;
}

int main(void) {
	return baz(2);
}
