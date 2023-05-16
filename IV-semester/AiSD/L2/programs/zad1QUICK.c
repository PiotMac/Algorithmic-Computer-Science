#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

int comparisons = 0;
int swaps = 0;
int* global_array;
int global_size;
bool print = false;

void swap(int *a, int *b) {
  if (a == b) {
  	return;
  }
  int temp = *a;
  *a = *b;
  *b = temp;
  swaps++;
}

bool isSorted(int arr[], int n) {
	for (int i = 0; i < n - 1; i++) {
		if (arr[i] > arr[i + 1]) {
			return false;
		}
	}
	return true;
}

void printArray(int arr[], int size) {
  for (int i = 0; i < size; ++i) {
    if (arr[i] < 10) {
	printf("0");
    }
    printf("%d  ", arr[i]);
  }
  printf("\n");
}

int partition(int array[], int low, int high) {
  int pivot = array[high];
  int i = (low - 1);

  for (int j = low; j < high; j++) {
    comparisons++;
    if (array[j] <= pivot) {
        i++;
      	swap(&array[i], &array[j]);
      }
  }

  swap(&array[i + 1], &array[high]);
  
  if (print) {
    	for (int i = 0; i < global_size; i++) {
    		if (global_array[i] != array[i]) {
    			global_array[i] = array[i];
    		}
    	}
    	printArray(global_array, global_size);
  }
  
  return (i + 1);
}

void quickSort(int array[], int low, int high) {
  if (low < high) {
    int pi = partition(array, low, high);
    quickSort(array, low, pi - 1);
    quickSort(array, pi + 1, high);
  }
}

int main() {
	int size;
	scanf("%d", &size);
	global_size = size;
	global_array = calloc(global_size, sizeof(int));
	if (size < 40) {
		print = true;
	}
	int input[size];
	for (int i = 0; i < size; i++) {
		scanf("%d", &input[i]);
		global_array[i] = input[i];
	}
	if (print) {
  		printf("NOT sorted array:\n");
  		printf("############################################################################################################################################\n");
  		printArray(input, size);
  		printf("############################################################################################################################################\n");
  		printf("\n");
  	}
	quickSort(input, 0, size - 1);
  	if (print) {
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
