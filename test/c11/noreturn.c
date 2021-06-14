#include <stdnoreturn.h>

void noreturn forever(void);

_Noreturn void forever(void) {
	int i = 0;
	while (1)
		i++;
}

int main(void) {
	return 0;
	forever();
}
