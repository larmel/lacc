int puts(const char *);

/* Extern inline should produce a symbol. */
extern inline int foo(int a) {
	return puts("Hello");
}

int main(void) {
	return 0;
}
