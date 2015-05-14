int main() {
    char a = 1, b = 2;

    int t1 = (a == 0 && (b = 0));
    int t2 = a || 1 || (b = 1);

    return b;
}
