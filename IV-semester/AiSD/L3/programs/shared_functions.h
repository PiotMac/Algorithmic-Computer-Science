#include <stdio.h>
#include <stdbool.h>

int comparisons = 0;
int swaps = 0;
int* global_array;
int global_size;


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

bool isSorted(int arr[], int n) {
	for (int i = 0; i < n - 1; i++) {
		if (arr[i] > arr[i + 1]) {
			return false;
		}
	}
	return true;
}

void swap(int *a, int *b) {
  int temp = *a;
  *a = *b;
  *b = temp;
  swaps++;
}

void rotate3(int *a, int *b, int *c) {
	int temp = *a;
	*a = *b;
	*b = *c;
	*c = temp;
	swaps += 3;
}
