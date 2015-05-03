
int main()
{
    int a;
    int *b = &a;

    *b = 2;
    a = *b;

    return a + *b;
}
