static int a = 42, *b = &a;

int *foo[] = {&a, &a, (int *) &b};

int main(void) {
	static int f = 3, *p = &f;

	return *b + *foo[1] + *p;
}
