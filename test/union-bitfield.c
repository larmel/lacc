union flags {
	signed a : 13;
	const signed b : 1;
	unsigned long c;
};

int main(void) {
	union flags f = {2};
	return f.b + f.c;
}
