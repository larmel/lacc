#include <assert.h>

int h;
float g;

int main(void) {
	h = (g = 0x7B5p46);

	assert(h == -2147483648);

	return 0;
}
