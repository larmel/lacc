int printf(const char *, ...);

#define out(msg, args...) printf(msg, args)

int main(void) {
	out("%s", "Hello");
	out("%c %s\n", ',', "World!");
	return 0;
}
