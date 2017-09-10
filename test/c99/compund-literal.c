int printf(const char *, ...);

int *a = (int []) {1, 2, 3, 4, 5},
    *b = &(&((int []) {1, 2, 3, 4, 5}[2]))[1];

struct point {
	int x, y;
};

int draw(struct point p) {
	return printf("(%d, %d)\n", p.x, p.y);
}

int main(void) {
	struct point
		p = (struct point) {5, 7},
		*q = &(struct point) {3, 14};

	(((struct point) {.y = 3})) = (struct point) {.x = 2} = *q;
	((struct point) {0}).y = 42;

	draw(p);
	draw(*q);
	draw((struct point) {.y = 1});
	draw((const struct point) { 0 });
	draw((const struct point[]) {[1] = {.y = 5, .x = 42}}[1]);

	return printf("%d, %d\n", *a, *b);
}
