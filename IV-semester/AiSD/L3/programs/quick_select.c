#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include "shared_functions.h"

//int* global_array;
//int global_size;
bool print = false;

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
	}
}

// Function to find median of an array of size 5 or less
int findMedianSelect(int arr[], int low, int high) {
    int size = high - low + 1;
    for (int i = 0; i < size - 1; i++) {
        for (int j = i + 1; j < size; j++) {
            comparisons++;
            if (arr[low + i] > arr[low + j]) {
                swap(&arr[low + i], &arr[low + j]);
            }
        }
    }
    return arr[(low + high) / 2];
}

// Function to find median of medians
int medianOfMediansSelect(int arr[], int low, int high) {
    int n = high - low + 1;
    if (n <= 5) {
        return findMedianSelect(arr, low, high);
    }
    int numGroups = (n + 4) / 5;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * 5;
        int groupEnd = (groupStart + 4 < n) ? (groupStart + 4) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedianSelect(arr, low + groupStart, low + groupStart + groupSize - 1);
    }
    //printArray(medians, numGroups);
    return medianOfMediansSelect(medians, 0, numGroups - 1);
}

// Function to partition the array around a pivot
int partitionSelect(int arr[], int low, int high, int pivot) {
    int i;
    
    for (i = low; i <= high; i++) {
        comparisons++;
        if (arr[i] == pivot) {
            break;
        }
    }
    swap(&arr[i], &arr[high]);
    
    i = low - 1;
    for (int j = low; j < high; j++) {
        comparisons++;
        if (arr[j] <= pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

void quickSort(int array[], int low, int high) {
  if (low < high) {
    int median_of_medians_pivot = medianOfMediansSelect(array, low, high);
    int pi = partitionSelect(array, low, high, median_of_medians_pivot);
    if (print) {
        printArray(array, global_size);
    }
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
	
	if (isSorted(input, size)) {
		printf("SORTING SUCCESSFUL!\n");
	}
	else {
		printf("Sorting NOT successful!\n");
	}
  return 0;
}