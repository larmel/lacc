int puts(const char *);

int noop(void);

int foo(char *list[5]) {
	puts(list[0]);
	puts(list[1]);
	return sizeof(list);
}

int prints(int n, ...) {
	return n;
}

char s1[] = "Hello";
char *s2 = "World";

int main() {
	int size = 0;
	char *words[2];
	words[0] = s1;
	words[1] = s2;

	size = foo(words);
	size = size + sizeof(words);
	size = size + prints(2, "hei", "hoi");

	return size;
}
