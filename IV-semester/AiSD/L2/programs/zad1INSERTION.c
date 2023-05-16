#include <stdio.h>
#include <stdbool.h>

int comparisons = 0;
int swaps = 0;

void printArray(int arr[], int size) {
	for (int i = 0; i < size; i++) {
		if (arr[i] < 10) {
			printf("0");
		}
		printf("%d  ", arr[i]);
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

void insertionSort(int arr[], int n) {
	int key, j;
	for (int i = 1; i < n; i++) {
		key = arr[i];
		j = i - 1;
		comparisons++;
		while (j >= 0 && arr[j] > key) {
			arr[j + 1] = arr[j];
			j = j - 1;
			swaps++;
			comparisons++;
		}
		arr[j + 1] = key;
		if (n < 40) {
			if (i < 10) {
				printf("ITERATION  %d: ", i);
			}
			else {
				printf("ITERATION %d: ", i);
			}
			printArray(arr, n);
		}
	}
}

int main() {
	int size;
	scanf("%d", &size);
	int input[size];
	for (int i = 0; i < size; i++) {
		scanf("%d", &input[i]);
	}
	
	if (size < 40) {
		printf("NOT sorted array:\n");
		printf("############################################################################################################################################\n");
		printArray(input, size);
		printf("############################################################################################################################################\n");
		printf("\n");
	}
	
	insertionSort(input, size);
	
	if (size < 40) {
		printf("\n");
		printf("Sorted array:\n");
		printf("############################################################################################################################################\n");
		printArray(input, size);
		printf("############################################################################################################################################\n");
		printf("\n");
	}
	
	printf("Number of comparisons: %d\n", comparisons);
	printf("Number of swaps: %d\n", swaps);
	
	if (isSorted(input, size) == true) {
		printf("SORTING SUCCESSFUL!\n");
	}
	else {
		printf("Sorting NOT successful!\n");
	}
	
	return 0;
}

