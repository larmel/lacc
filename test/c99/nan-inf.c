#include <assert.h>
#include <stdlib.h>

static int test_nan(void) {
	int c = 0;
	float fnan = strtof("NAN()", NULL);
	double dnan = strtod("NAN()", NULL);
	long double ldnan = strtold("NAN()", NULL);

	assert(fnan != 0);
	if (fnan) {
		c++;
	} else if (!fnan) {
		assert(0);
	}

	assert(dnan != 0);
	if (dnan) {
		c++;
	} else if (!dnan) {
		assert(0);
	}

	assert(ldnan != 0);
	if (ldnan) {
		c++;
	} else if (!ldnan) {
		assert(0);
	}

	return c == 3;
}

static int test_inf(void) {
	int c = 0;
	float finf = strtof("INF()", NULL);
	double dinf = strtod("INF()", NULL);
	long double ldinf = strtold("INF()", NULL);

	assert(finf != 0);
	if (finf) {
		c++;
	} else if (!finf) {
		assert(0);
	}

	assert(dinf != 0);
	if (dinf) {
		c++;
	} else if (!dinf) {
		assert(0);
	}

	assert(ldinf != 0);
	if (ldinf) {
		c++;
	} else if (!ldinf) {
		assert(0);
	}

	return c == 3;
}

int main(void) {
	assert(test_nan());
	assert(test_inf());
	return 0;
}
