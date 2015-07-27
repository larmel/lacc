char *name;

int main() {
	char text[] = "ab";
	name = text;

	*name = '4';

	return name[0];
}
