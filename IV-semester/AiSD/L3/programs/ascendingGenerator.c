#include "shared_functions.h"
#include <stdlib.h>

int main(int argc, char* argv[]) {
	int n = atoi(argv[1]);
	if (n <= 0) {
		printf("Wrong input!");
		return 0;
	}
	int randoms[n];
	randoms[0] = 0;
	for (int i = 2; i <= n; i++) {
		randoms[i - 1] = 2 * i - 1;
	}

  	printf("%d ", n);
  	printArray(randoms, n);
  	return 0;
}