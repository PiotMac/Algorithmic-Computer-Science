#include <stdio.h>
#include <stdlib.h>

void printArray(int arr[], int n) {
	for (int i = 0; i < n; i++) {
		printf("%d ", arr[i]);
	}
	printf("\n");
}

int main(int argc, char* argv[]) {
	int n = atoi(argv[1]);
	if (n <= 0) {
		printf("Wrong input!");
		return 0;
	}
	int randoms[n];
	randoms[0] = 2 * n - 1;
	for (int i = n; i >= 2; i--) {
		randoms[n - i + 1] = 2 * i - 4;
	}
	/*
	randoms[0] = 8;
	randoms[1] = 5;
	randoms[2] = 9;
	randoms[3] = 2;
	randoms[4] = 6;
	randoms[5] = 3;
	*/
	
  	printf("%d ", n);
  	printArray(randoms, n);
  	return 0;
}
