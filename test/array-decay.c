int printf(const char *, ...);

struct point {
	int x, y;
};

struct point p[3] = {{1, 2}, {3, 4}, {5, 6}};

int foo(int n, struct point arr[n]) {
	return arr->y;
}

int main(void) {
	int a = p->y;
	int b = foo(3, p);
	return printf("%d, %d\n", a, b);
}
