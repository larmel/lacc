int puts(const char *);

const char foo[] = "\041\042 \043\052 The answer to everything is \x34\x32 (\?)";

char bar[] = "\"The End\"\0 Nothing to see here.";

int main() {
	puts(foo);
	puts(bar);
	return 0;
}
