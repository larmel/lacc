int main() {
    int a = sizeof(volatile char);
    int b = sizeof (a + 2);
    return sizeof a + sizeof(int (*)(char)) + sizeof(struct {char a;});
}
