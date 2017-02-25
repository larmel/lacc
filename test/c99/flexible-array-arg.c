int printf(const char *, ...);

struct flex {
	int a;
	char b;
	char s[];
} foo = {1};

struct flex ret(void) {
	struct flex p = {3, 2};
	return p;
}

int arg(struct flex p) {
	return printf("%d, %d\n", p.a, p.b);
}

int main(void) {
	struct flex p = ret();
	return arg(p) + arg(foo);
}
