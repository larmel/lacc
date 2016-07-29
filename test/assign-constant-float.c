int printf(const char *, ...);

int h;
float g;

int main(void) {
	h = (g = 0x7B5p46);
	return printf("%d\n", h);
}
