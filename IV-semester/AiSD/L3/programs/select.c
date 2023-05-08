#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "shared_functions.h"

int* input;

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
int findMedianSelect(int arr[], int size) {
    for (int i = 0; i < size - 1; i++) {
        for (int j = i + 1; j < size; j++) {
            comparisons++;
            if (arr[i] > arr[j]) {
                swap(&arr[i], &arr[j]);
            }
        }
    }
    return arr[size / 2];
}

// Function to find median of medians
int medianOfMediansSelect(int arr[], int n) {
    if (n <= 5) {
        return findMedianSelect(arr, n);
    }
    int numGroups = (n + 4) / 5;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * 5;
        int groupEnd = (groupStart + 4 < n) ? (groupStart + 4) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedianSelect(arr + groupStart, groupSize);
    }
    //printArray(medians, numGroups);
    return medianOfMediansSelect(medians, numGroups);
}

// Function to partition the array around a pivot
int partitionSelect(int arr[], int n, int pivot) {
    int i;
    for (i = 0; i < n; i++) {
        comparisons++;
        if (arr[i] == pivot) {
            break;
        }
    }
    swap(&arr[i], &arr[n - 1]);
    i = 0;
    for (int j = 0; j < n - 1; j++) {
        comparisons++;
        if (arr[j] <= pivot) {
            swap(&arr[i], &arr[j]);
            i++;
        }
    }
    swap(&arr[i], &arr[n - 1]);
    return i;
}

// Function to find the kth smallest element using median of medians
int selectAlgorithm(int arr[], int n, int k) {
    if (n == 1) {
        return arr[0];
    }
    int pivot = medianOfMediansSelect(arr, n);
    int pivotIndex = partitionSelect(arr, n, pivot);
    if (global_size < 50) {
        printArray(input, global_size);
    }
    if (k == pivotIndex) {
        return arr[pivotIndex];
    } else if (k < pivotIndex) {
        return selectAlgorithm(arr, pivotIndex, k);
    } else {
        return selectAlgorithm(arr + pivotIndex + 1, n - pivotIndex - 1, k - pivotIndex - 1);
    }
}

// Test the select function
int main() {
    srand(time(NULL));
    int position;
    //Taking n
	scanf("%d", &global_size);
    input = calloc(global_size, sizeof(int));
	//input[global_size];
	global_array = calloc(global_size, sizeof(int));
    //Taking k-th position statistics
    scanf("%d", &position);
    if (position > global_size || position < 1) {
        printf("Given position is out of bounds!\n");
        return 0;
    }
    //Taking n keys
	for (int i = 0; i < global_size; i++) {
		scanf("%d", &input[i]);
		global_array[i] = input[i];
	}
	
	if (global_size < 50) {
		printf("NOT sorted array:\n");
		printf("############################################################################################################################################\n");
		printArray(input, global_size);
		printf("############################################################################################################################################\n");
		printf("\n");
	}
	
	int found_statistic = selectAlgorithm(input, global_size, position - 1);
    int local_comparisons = comparisons;
    int local_swaps = swaps;
	
	if (global_size < 50) {
        printf("\nArray after Select:\n");
		printf("############################################################################################################################################\n");
		printArray(input, global_size);
        printf("############################################################################################################################################\n");
		printf("\n");

        insertionSort(input, global_size);

		printf("Sorted array:\n");
		printf("############################################################################################################################################\n");
		printArray(input, global_size);
		printf("############################################################################################################################################\n");
		printf("\n");


        printf("%d-th element of the given array is: %d\n", position, found_statistic);

	}
    if (input[position - 1] == found_statistic) {
	    printf("SUCCESS!\n");
	}
    else {
	    printf("FAILURE!\n");
    }
    printf("\n");
	printf("Number of comparisons: %d\n", local_comparisons);
	printf("Number of swaps: %d\n", local_swaps);
	
	return 0;
}

