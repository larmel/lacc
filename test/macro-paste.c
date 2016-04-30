#define FOO(s) s ## _f ## u ## nc
#define STR(s) #s
#define CAT(a, b) STR(a ## b)

int puts(const char *);

int foo_func(void)
{
	return puts(CAT(foo, 5));
}

int main(void)
{
	return FOO(foo)();
}
