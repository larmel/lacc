#define FOO(a, b, c) (a)
#define CONCAT(x, y, z) x z ## y + 1
#define STR(i, j) #i "hello" #j "world"

int printf(const char *, ...);

int main(void) {
	int a = CONCAT(2,,);
	int b = CONCAT(,2,);
	int c = FOO(0 + a,  ,);
	return printf(STR(,c)) + c;
}
