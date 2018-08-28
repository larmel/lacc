
int foo(int a[restrict], int b[const], int c[restrict const]) {
	return *a + *b + *c;
}

int main(void) {
	int arr[] = {1, 2, 3};
	return foo(arr, arr + 1, arr + 2);
}
