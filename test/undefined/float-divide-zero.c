#include <assert.h>
#include <stdio.h>
#include <string.h>

float a = 1.0/0.0;
float b = (0.0/0.0) / 0.0;
float c = (2.1/-0.0) / 0.0;
float d = 0.0 / (2.1/0.0);
float e = 1.0 / (2.1/-0.0);

static char str[128];

int main(void) {
	sprintf(str, "a = %f, b = %f, c = %f, d = %f, e = %f", a, b, c, d, e);

	assert(!strcmp("a = inf, b = -nan, c = -inf, d = 0.000000, e = -0.000000", str));

	return 0;
}
