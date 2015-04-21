int puts(const char *);

#define FOO(a, b) (a b - 1)

int main() {
	int a = 42;

	puts(__FILE__);
	puts(__func__);
	return __LINE__ + FOO ( 1 + a *, 2 );
}
