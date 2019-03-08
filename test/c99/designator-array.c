int printf(const char *, ...);

int a[2][3] = {[1] = {1, 2, 3}, [0][1] = 1},
	b[2][3] = {[1][0] = 4, 3, 2},
	c[2][3] = {4, [1] = 7, 9},
	d[2][3] = {{1, 2, 3}, [0] = 2, [1][1] = 7};

int p1(int a[][3]) {
	return printf("{{%d, %d, %d}, {%d, %d, %d}}\n",
		a[0][0], a[0][1], a[0][2], a[1][0], a[1][1], a[1][2]);
}

void p2(char a[], int n) {
	int i;
	printf("{");
	for (i = 0; i < n; ++i) {
		printf("%d", a[i]);
		if (i < n - 1) {
			printf(", ");
		}
	}
	printf("}\n");
}

struct {
	char c[4];
	int d;
} s1 = { {1, [0] = 3, 2, 3, 4, [3] = 7}, 42 };

char s2[] = {[1] = 2, [42] = 3, [6] = 2};

int main(void) {
	int i;
	char
		foo[] = {'1', '8', [9] = '5', '-'},
		bar[] = {[42] = 1};

	p2(foo, sizeof(foo) - 1);
	p2(bar, sizeof(bar) - 1);

	printf("s1[0]: {[%d, %d, %d, %d], %d}\n",
		s1.c[0], s1.c[1], s1.c[2], s1.c[3], s1.d);

	printf("sizeof(s2) = %lu, [", sizeof(s2));
	for (i = 0; i < sizeof(s2); ++i) {
		printf("%d", s2[i]);
		if (i > 0) {
			printf(", ");
		}
	}
	printf("]\n");

	return p1(a) + p1(b) + p1(c) + p1(d);
}
