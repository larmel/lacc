int printf(const char *, ...);

union value {
	int i;
	float f;
} a = {.f = 3.14},
  b = {8, .f = 2.71, },
  c = {.i = 6},
  d = {.i = 6, .i = 4, .f = 5.13},
  e = {3,};

int p1(union value u) {
	return printf("(%d, %f)\n", u.i, u.f);
}

union U1 {
	struct S1 {
		int x;
		char y[2];
	} u;
	union value v;
} t1 = {.u = {1, 2}},
  t2 = {.v = {3}},
  t3 = {.v = {.f = 2.71}},
  t4 = {42, 1, .v.f = 2.71};

int p2(union U1 u) {
	return printf("{{%d, {%d, %d}}, ", u.u.x, u.u.y[0], u.u.y[1]) + p1(u.v);
}

int main(void) {
	union value f = {.f = 0.2f};
	union U1
		t5 = {1, .v.i = 3,},
		t6 = {42, {13, 19}},
		t7 = {.v.f = 4.44, .u.y[1] = 3,};

	return p1(a) + p1(b) + p1(c) + p1(d) + p1(e) + p1(f)
		+ p2(t1) + p2(t2) + p2(t3) + p2(t4) + p2(t5) + p2(t6) + p2(t7);
}
