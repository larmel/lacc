#include <assert.h>

static int a __asm__ ("_A");

static int a = 42;

int b = 2, c __asm__("_C") = 33;

int foo(int mode) __asm("__" "foo");

int foo(int mode)
{
    static int d __asm__("__D") = 50;

    return mode + d;
}

int bar() __asm__("BAR");

int bar(mode)
int mode;
{
    return mode + c;
};

int main(void) {
    assert(a == 42);
    assert(c == 33);
    assert(foo(1) == 51);
    assert(bar(2) == 35);
    return 0;
}
