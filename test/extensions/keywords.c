#include <assert.h>


static __inline void foo(char * __restrict p, char  * __restrict__ q) {
	unsigned char x = (unsigned char) 0xff;
	__signed char a = (__signed__ char) x;

	assert(a == -1);
	assert(p != q);
}

char text[] = "Hello world!";
char * __volatile p = text, * __volatile__ q = text + 3;

int main(void) {
	foo(p, q);
	return 0;
}

