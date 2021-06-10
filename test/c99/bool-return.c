#include <assert.h>
#include <stdbool.h>

bool is_interesting(char c) {
	switch (c) {
	case 'a':
		return false;
	case 'b':
		return true;
	default:
		return 1;
	}
}

int main(void) {
	assert(!is_interesting('a'));
	assert(is_interesting('b'));
}
