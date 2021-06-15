int printf(const char *, ...);

int main(void) {
	float f1 = 5312.4f, f2 = -12312.14f;
	double d1 = 431235311.92340;

	char c = f2;
	unsigned char u = f2;
	short s = d1;
	int i = f1;
	long l = d1;

	return printf("c = %d, u = %u, s = %d, i = %d, l = %ld\n", c, u, s, i, l);
}
