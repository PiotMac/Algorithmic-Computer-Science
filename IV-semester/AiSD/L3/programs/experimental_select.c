#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "shared_functions.h"

void insertionSortExp(int arr[], int n) {
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

// Function to find median of an array of size of divider or less
int findMedianExp(int arr[], int size) {
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
int medianOfMediansExp(int arr[], int n, int divider) {
    if (n <= divider) {
        return findMedianExp(arr, n);
    }
    int numGroups = (n + (divider - 1)) / divider;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * divider;
        int groupEnd = (groupStart + (divider - 1) < n) ? (groupStart + (divider - 1)) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedianExp(arr + groupStart, groupSize);
    }
    //printArray(medians, numGroups);
    return medianOfMediansExp(medians, numGroups, divider);
}

// Function to partition the array around a pivot
int partitionExp(int arr[], int n, int pivot) {
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
int selectExperimental(int arr[], int n, int k, int divider) {
    if (n == 1) {
        return arr[0];
    }
    int pivot = medianOfMediansExp(arr, n, divider);
    int pivotIndex = partitionExp(arr, n, pivot);

    if (k == pivotIndex) {
        return arr[pivotIndex];
    } else if (k < pivotIndex) {
        return selectExperimental(arr, pivotIndex, k, divider);
    } else {
        return selectExperimental(arr + pivotIndex + 1, n - pivotIndex - 1, k - pivotIndex - 1, divider);
    }
}

// Test the select function
int main() {
    srand(time(NULL));
    int position;
    //Taking n
	scanf("%d", &global_size);
	int input[global_size];
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
	
	int found_statistic = selectExperimental(input, global_size, position - 1, 3);
    int local_comparisons = comparisons;
    int local_swaps = swaps;
	
	if (global_size < 50) {
        printf("Array after Experimental Select:\n");
		printf("############################################################################################################################################\n");
		printArray(input, global_size);
        printf("############################################################################################################################################\n");
		printf("\n");

        insertionSortExp(input, global_size);

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