int puts(const char *);

#define STR(s) #s

int main(void) {
	int n = 0;

	n += puts(STR(   this   is some   string  ));
	n += puts(STR(this -= '8' 2u '\a' "i\ns\t"
		'\n' a "te\0st"));

	return n;
}
