int puts(const char *s);

const char str[] = "\tconst \af \but \vat \fak \x64 \\? \? \' \" ' \r\n";

int main(void) {
	puts(str);
	return sizeof(str);
}
