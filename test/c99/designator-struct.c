int printf(const char *, ...);

struct obj {
	int x;
	int y[3];
	int z;
} a = {.y = 4, 5, .x = 1},
  b = {.y[1] = 3, 6},
  c = {.z = 3, .x = 2, 5, 6, 7};

int p1(struct obj o) {
	return printf("{%d, {%d, %d, %d}, %d}\n",
		o.x, o.y[0], o.y[1], o.y[2], o.z);
}

struct S1 {
	int i;
	union {
		char c;
		float f;
		long l;
	};
	short j;
} t1 = {5, 1, 2},
  t2 = {.f = 3.2f, 7,},
  t3 = {0, {0}, .c = 'a', 5};

static int p2(struct S1 s) {
	return printf("{%d, {%d, %f, %ld}, %d}\n", s.i, s.c, s.f, s.l, s.j);
}

int main(void) {
	return p1(a) + p1(b) + p1(c)
		+ p2(t1) + p2(t2) + p2(t3);
}
