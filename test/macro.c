int puts(const char *);

int main() {
	puts(__FILE__);
	puts(__func__);
	return __LINE__;
}
