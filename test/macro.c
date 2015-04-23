int puts(const char *);

#define FOO(a, b) (a b - 1)

#define BAR(x) __FILE__, *d = #x, e = x ;

int main() {
	int a = 42;
	char *c = BAR(__LINE__)

	puts(c);
	puts(d);

	puts(__FILE__);
	puts(__func__);
	return __LINE__ + FOO ( 1 + a *, 2 ) + e;
}
