#include <assert.h>

#define init(x) (__builtin_constant_p(x) ? x : -1)

int a = 5;

static int data[] = {
	init(3 + 9),
	init(8 - a),
	init(0 ? a : 4)
};

int main(void) {
	assert(data[0] == 12);
	assert(data[1] == -1);
	assert(data[2] == 4);
	return 1;
}
