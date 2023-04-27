#include <stdio.h>
#include <signal.h>

void signals_handler(int dummy) {}

int main() {
	for (int i = 1; i <= 64; i++) {
		if (signal(i, signals_handler) == SIG_ERR) {
			printf("Signal %d cannot be handled!\n", i);
		}
	}
	return 0;
}
