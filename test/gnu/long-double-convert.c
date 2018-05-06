int printf(const char *, ...);

int tofloat(long double ld) {
	float a = ld;
	double b = ld;
	return printf("%f, %f\n", a, b);
}

int toint(long double ld) {
	char a = ld;
	unsigned char b = ld;
	int c = ld;
	unsigned int d = ld;
	long e = ld;
	unsigned long f = ld;
	return printf("%d, %d, %d, %u, %ld, %lu\n", a, b, c, d, e, f);
}

int main(void) {
	long double d = 3.14, e = 145260912182745.12486L, f = -972316.70L;
	return toint(d) + toint(e) + toint(f)
		+ tofloat(d) + tofloat(e) + tofloat(f);
}
