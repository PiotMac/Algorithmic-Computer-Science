#include <stdio.h>
#include <stdlib.h>

void printArray(int arr[], int size) {
	for (int i = 0; i < size; i++) {
		if (arr[i] < 10) {
			printf("0%d  ", arr[i]);
		}
		else {
			printf("%d  ", arr[i]);
		}
	}
	printf("\n");
}


int main(int argc, char* argv[]) {
	int n = atoi(argv[1]);
	if (n <= 0) {
		printf("Wrong input!");
		return -1;
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