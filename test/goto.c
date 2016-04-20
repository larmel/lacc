int main(void) {
	int i = 0;

start:
	while (i < 100) {
		if (i == 42) goto end;
		i++;
		goto start;
		i++;
	}

end:
	return i;
}
