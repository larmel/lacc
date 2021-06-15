int printf(const char *, ...);

int foo(void *ptr) {
	return printf("foo\n");
}

int main(void) {
	void (*bar)(void *) = (void (*)(void *)) foo;

	bar(0);
	return 0;
}
