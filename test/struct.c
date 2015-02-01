typedef struct {
	int x;
	int y;
} point_t;

int main() {
	point_t point;
	point_t *foo;

	point.x = 1;
	point.y = 2;

	foo = &point;
	foo->x = 2;

	return point.x + point.y;
}
