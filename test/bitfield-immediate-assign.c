int printf(const char *, ...);

int h, i;
unsigned j;

struct {
	signed f0 : 20;
	unsigned f1 : 20;
} f;

int main(void) {
	i = (f.f0 = (h = 0x20CF9BFEL));
	j = (f.f1 = (h = 0x20CF9BFEL));
	return printf("%d, %u\n", i, j);
}
