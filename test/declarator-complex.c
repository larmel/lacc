int puts(const char *);

static char *str;

static void func(void) {
	puts(str);
}

static void (*getfunc(char *s))(void) {
	str = s;
	return func;
}

int main(void) {
	void (*foo)(void) = getfunc("Hello World!");
	foo();
	return 0;
}
