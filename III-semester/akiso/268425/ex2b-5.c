#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

int main() {
	for (int i = 0; i <= 64; i++) {
		if (kill(1, i) == 0) {
			printf("Signal %d has been sent!\n", i);
		}
		else {
			printf("Signal %d has NOT been sent!\n", i);
		}	
	}
	return 0;
}

