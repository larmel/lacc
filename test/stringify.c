#define STR(s) #s

int puts(const char *);

int main(void) {
	return puts(STR(   this   is some   string  ));
}
