#define name(s) s ## _f ## u ## nc

int foo_func()
{
	return 1;
}

int main()
{
	return name(foo)();
}
