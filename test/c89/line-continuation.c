int printf(const char *, ...);

char foo[] = "a\\
\";

char c1 = '\
\
a\
';

char c2 = '\
\\\
';

/* Trigraph backslash */
char c3 = '??/
a??/
';

int main(void) {
	printf("%s", foo);
	printf("%c, %c, %c\n", c1, c2, c3);
	return sizeof(foo);
}
