int printf(const char *, ...);

static int test1(void) {
	struct A { char text[4]; } a = {"Hei"};
	struct B { int i; } b = {42};
	union U {
		struct A a;
		struct B b;
	} u1 = {a},
	  u2 = {.b = b};
	struct C {
		union U u;
	} s1 = {u1},
	  s2 = {a},
	  s3 = {"Oi"};

	printf("%s\n", u1.a.text);
	printf("%d\n", u2.b.i);

	printf("%s\n", s1.u.a.text);
	printf("%s\n", s2.u.a.text);
	printf("%s\n", s3.u.a.text);
	return 0;
}

static int test2(void) {
	struct A { unsigned char data; } a = {'c'};
	struct B {
		struct A a;
		int i;
	} b = {a, 42};
	struct C {
		struct B b;
	} c = {b};

	return printf("{{{%c}, %d}}\n", c.b.a.data, c.b.i);
}

int main(void) {
	return test1() + test2();
}
