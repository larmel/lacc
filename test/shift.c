#include <stdio.h>

void shift_unsigned_char(void)
{
	unsigned char a = 0xE8u;
	char b = 2;

	printf("%lu\n", sizeof(a << b));
	printf("%d\n", a << b);
	printf("%d\n", a >> b);
}

void shift_signed_char(void)
{
	signed char a = 0x65;
	int b = 3;

	printf("%lu\n", sizeof(a << b));
	printf("%d\n", a << b);
	printf("%d\n", a >> b);
}

void shift_signed_long(void)
{
	long a = 0x7E00FF0F;
	short b = 33;

	printf("%lu\n", sizeof(a << b));
	printf("%ld\n", a << b);
	printf("%ld\n", a >> 4);
}

void shift_chained(void)
{
	unsigned a = 0x7EABCDF4;
	printf("%u\n", a << 3 >> 1);
}

int main() {
	shift_unsigned_char();
	shift_signed_char();
	shift_signed_long();
	shift_chained();
	return 0;
}
