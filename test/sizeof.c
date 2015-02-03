int main() {
	int bar[5];
	long a = sizeof(volatile char) + sizeof(bar);
	long b = sizeof (a = a + 2);
	return sizeof a + sizeof(int (*)(char)) + sizeof(struct {char a;}) + b + a;
}
