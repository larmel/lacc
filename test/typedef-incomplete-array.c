#include <stdio.h>

static void test_typedef()
{
    typedef int A[];
    A a = { 1, 2 };
    A b = { 3, 4, 5 };
    printf("exp:%d, val:%d\n", 2, sizeof(a) / sizeof(*a));
    printf("exp:%d, val:%d\n" ,3, sizeof(b) / sizeof(*b));
}

int main(void)
{
    test_typedef();
    return 0;
}

