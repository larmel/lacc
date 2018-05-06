int printf(const char *, ...);

int float_to_unsigned(void) {
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

	return printf(
		"unsigned char: (%u, %u, %u, %u)\n"
		"unsigned short: (%u, %u, %u, %u)\n"
		"unsigned int: (%u, %u, %u, %u)\n"
		"unsigned long: (%lu, %lu, %lu, %lu)\n",
		float_char_of, float_char_uf, double_char_of, double_char_uf,
		float_short_of, float_short_uf, double_short_of, double_short_uf,
		float_int_of, float_int_uf, double_int_of, double_int_uf,
		float_long_of, float_long_uf, double_long_of, double_long_uf);
}

int float_to_signed(void) {
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

	return printf(
		"char: (%d, %d, %d, %d)\n"
		"short: (%d, %d, %d, %d)\n"
		"int: (%d, %d, %d, %d)\n"
		"long: (%ld, %ld, %ld, %ld)\n",
		float_char_of, float_char_uf, double_char_of, double_char_uf,
		float_short_of, float_short_uf, double_short_of, double_short_uf,
		float_int_of, float_int_uf, double_int_of, double_int_uf,
		float_long_of, float_long_uf, double_long_of, double_long_uf);
}

int main(void) {
	return float_to_unsigned() + float_to_signed();
}
