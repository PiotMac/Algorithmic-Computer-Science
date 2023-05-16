#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

int comparisons = 0;
int swaps = 0;

int global_size;
int* global_array;
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

void insertionSort(int arr[], int low, int high) {
	int key, j;
	for (int i = low + 1; i <= high; i++) {
		key = arr[i];
		j = i;
		comparisons++;
		while (j > low && arr[j - 1] > key) {
			arr[j] = arr[j - 1];
			j = j - 1;
			swaps++;
			comparisons++;
		}
		arr[j] = key;
        if (print) {
    	for (int i = 0; i < global_size; i++) {
    		if (global_array[i] != arr[i]) {
    			global_array[i] = arr[i];
    		}
    	}
    	printArray(global_array, global_size);
  }
	}
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

void hybridSort(int array[], int low, int high) {
    if (low < high)
    {
        if (high - low <= 5) {
            insertionSort(array, low, high);
        }
        else {
            int pivot = partition(array, low, high);
            hybridSort(array, low, pivot - 1);
            hybridSort(array, pivot + 1, high);
        }
    }
}

int main() {
	int size;
	scanf("%d", &size);
    if (size < 40) {
        print = true;
    }
    global_size = size;
	global_array = calloc(global_size, sizeof(int));
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
	
	hybridSort(input, 0, size - 1);
	
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