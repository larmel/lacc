struct foo {
	char a;
	short b, c;
} bar = {1}, baz = {2};

int main(void) {
	bar = baz;
	return bar.a;
}
