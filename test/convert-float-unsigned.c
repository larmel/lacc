int printf(const char *, ...);

#define LONG_SIGN_BIT (1ul << 63)
#define ULONG_MAX (0xFFFFFFFFFFFFFFFFul)
#define LONG_MAX (0x7FFFFFFFFFFFFFFFl)

int float_to_ushort(void) {
	float g = 0x92F9E84EL;
	unsigned short s = g;
	return printf("(float) unsigned short: %d\n", s);
}

int float_to_uint(void) {
	float f1 = 1.5678e30f, f2 = LONG_SIGN_BIT, f3 = ULONG_MAX;
	unsigned int a = f1, b = f2, c = f3;
	return printf("(float) unsigned int: %u, %u, %u\n", a, b, c);
}

int float_to_ulong(void) {
	float f1 = 1.5678e30f, f2 = LONG_SIGN_BIT, f3 = ULONG_MAX, f4 = LONG_MAX;
	unsigned long a = f1, b = f2, c = f3, d = f4;
	return printf("(float) unsigned long: %lu, %lu, %lu, %lu\n", a, b, c, d);
}

int double_to_uint(void) {
	double d1 = 1.67890e20, d2 = LONG_SIGN_BIT, d3 = ULONG_MAX;
	unsigned int a = d1, b = d2, c = d3;
	return printf("(double) unsigned int %u, %u, %u\n", a, b, c);
}

int double_to_ulong(void) {
	double d1 = 1.67890e20, d2 = LONG_SIGN_BIT, d3 = ULONG_MAX;
	unsigned long a = d1, b = d2, c = d3;
	return printf("(double) unsigned long: %lu, %lu, %lu\n", a, b, c);
}

int main(void) {
	return float_to_ushort()
		+ float_to_uint()
		+ float_to_ulong()
		+ double_to_uint()
		+ double_to_ulong();
}
