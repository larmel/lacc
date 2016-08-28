int printf(const char *, ...);

int f = 0;

union {
	int f0 : 3;
	unsigned f1 : 2;
} b = {0x04};

int main(void) {
	if (b.f1) {
		f += 1;
	}

	if (b.f0 > 0) {
		f += 2;
	}

	return printf("answer = %d\n", f);
}
