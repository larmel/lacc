#include <assert.h>
#include <limits.h>

static int float_to_unsigned(void) {
	unsigned char
		float_char_of = (unsigned char) 289.0f,
		float_char_uf = (unsigned char) -41.0f,
		double_char_of = (unsigned char) 289.0,
		double_char_uf = (unsigned char) -41.0;
	unsigned short
		float_short_of = (unsigned short) 2234589.0f,
		float_short_uf = (unsigned short) -41.0f,
		double_short_of = (unsigned short) 2234589.0,
		double_short_uf = (unsigned short) -41.0;
	unsigned int
		float_int_of = (unsigned int) 5678134124.1f,
		float_int_uf = (unsigned int) -41.0f,
		double_int_of = (unsigned int) 5678134124.1,
		double_int_uf = (unsigned int) -41.0;
	unsigned long
		float_long_of = (unsigned long) 4.1e24f,
		float_long_uf = (unsigned long) -41.0f,
		double_long_of = (unsigned long) 4.1e24,
		double_long_uf = (unsigned long) -41.0;

	assert(float_char_of == 33);
	assert(float_char_uf == 215);
	assert(double_char_of == 33);
	assert(double_char_uf == 215);

	assert(float_short_of == 6365);
	assert(float_short_uf == 65495);
	assert(double_short_of == 6365);
	assert(double_short_uf == 65495);

	assert(float_int_of == 1383166976);
	assert(float_int_uf == 4294967255);
	assert(double_int_of == 1383166828);
	assert(double_int_uf == 4294967255);

	assert(float_long_of == 0);
	assert(float_long_uf == 18446744073709551575ul);
	assert(double_long_of == 0);
	assert(double_long_uf == 18446744073709551575ul);

	return 0;
}

static int float_to_signed(void) {
	char
		float_char_of = (char) 160.0f,
		float_char_uf = (char) -160.0f,
		double_char_of = (char) 160.0,
		double_char_uf = (char) -160.0;
	short
		float_short_of = (short) 2234589.0f,
		float_short_uf = (short) -2234589.0f,
		double_short_of = (short) 2234589.0,
		double_short_uf = (short) -2234589.0;
	int
		float_int_of = (int) 5678134124.1f,
		float_int_uf = (int) -5678134124.0f,
		double_int_of = (int) 5678134124.1,
		double_int_uf = (int) -5678134124.0;
	long
		float_long_of = (long) 4.1e24f,
		float_long_uf = (long) -4.1e24f,
		double_long_of = (long) 4.1e24,
		double_long_uf = (long) -4.1e24;

	assert(float_char_of == -96);
	assert(float_char_uf == 96);
	assert(double_char_of == -96);
	assert(double_char_uf == 96);

	assert(float_short_of == 6365);
	assert(float_short_uf == -6365);
	assert(double_short_of == 6365);
	assert(double_short_uf == -6365);

	assert(float_int_of == -2147483648);
	assert(float_int_uf == -2147483648);
	assert(double_int_of == -2147483648);
	assert(double_int_uf == -2147483648);

	assert(LONG_MIN == -9223372036854775807 - 1);
	assert(float_long_of == LONG_MIN);
	assert(float_long_uf == LONG_MIN);
	assert(double_long_of == LONG_MIN);
	assert(double_long_uf == LONG_MIN);

	return 0;
}

int main(void) {
	return float_to_unsigned() + float_to_signed();
}
