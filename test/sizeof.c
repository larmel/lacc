typedef struct Foo Bar;

int main(void) {
	int bar[5];
	long a = sizeof(volatile char) + sizeof(bar);
	long b = sizeof (a = a + 2);
	unsigned long c = sizeof(Bar *);
	unsigned long d = sizeof(int (*)(char)) + sizeof(struct {char a;});
	return sizeof a + a + b + c + d;
}
