int puts(const char *);

#define FOO(a, b) (a b - 1)

#define BAR(x) __FILE__, *d = #x, e = x ;

/*
 * Some comments to confuse line counting.
 */

int main() {
	int a = 42; /*     Single line comment. */
	char *c = BAR(
		__LINE__
	)

	puts(c);
	puts(d);

	puts(__FILE__);
#if (__STDC_VERSION__ >= 199901)
	puts(__func__);
#endif
	return __LINE__ + FOO ( 1 + a *, 2 ) + e;
}
