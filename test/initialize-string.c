int printf(const char *, ...);

char foo[5] = "Hei";

int main(void) {
	char bar[7] = ("wat");
	int check = sizeof(foo) + foo[4] + bar[6];
	return printf("foo: %s, bar: %s\n", foo, bar) + check;
}
