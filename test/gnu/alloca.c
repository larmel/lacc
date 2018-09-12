#include <alloca.h>
#include <string.h>
#include <stdio.h>

static int foo(int n, const char *str) {
	char *data = alloca(n);
	memcpy(data, str, n);

	return printf("%s\n", data);
}

static const char hello[] = "Hello world!";

int main(void) {
	return foo(sizeof(hello), hello);
}
