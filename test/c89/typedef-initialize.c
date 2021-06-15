#include <assert.h>

int printf(const char *, ...);

static void test1(void)
{
	typedef int A[];
	A a = { 1, 2 };
	A b = { 3, 4, 5 };

	assert(sizeof(a) / sizeof(*a) == 2);
	assert(sizeof(b) / sizeof(*b) == 3);
}

static void test2(void)
{
	typedef int A[];
	A a = { 1, 2 }, b = { 3, 4, 5 };

	assert(sizeof(a) / sizeof(*a) == 2);
	assert(sizeof(b) / sizeof(*b) == 3);
}

typedef char T[][2];

static T const g1 = {{'a', 'b'}, {'c'}};
T g2 = {'1', '2', 'a', 'b', '8', '0', '='};

static void test3(void)
{
	int i;

	printf("g1: ");
	for (i = 0; i < sizeof(g1) / sizeof(*g1); ++i) {
		printf("{%c, %c} ", g1[i][0], g1[i][1]);
	}

	printf("\n");
	printf("g2: ");
	for (i = 0; i < sizeof(g2) / sizeof(*g2); ++i) {
		printf("{%c, %c} ", g2[i][0], g2[i][1]);
	}

	printf("\n");
}

typedef const char S[];

static void test4(void)
{
	S s1 = "Hello world", s2 = {'h', 'i', '\0'};

	printf("%s, %lu\n", s1, sizeof(s1));
	printf("%s, %lu\n", s2, sizeof(s2));
}

int main(void)
{
	test1();
	test2();
	test3();
	test4();
	return 0;
}
