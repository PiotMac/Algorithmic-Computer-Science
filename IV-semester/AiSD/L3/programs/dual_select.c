#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "shared_functions.h"

int global_size = 0;
int* global_array;
bool print = false;

void insertionSort(int arr[], int low, int high) {
    int n = high - low + 1;
	int key, j;
	for (int i = 1; i < n; i++) {
		key = arr[low + i];
		j = i - 1;
		comparisons++;
		while (j >= 0 && arr[low + j] > key) {
			arr[low + j + 1] = arr[low + j];
			j = j - 1;
			swaps++;
			comparisons++;
		}
		arr[low + j + 1] = key;
	}
}

// Function to find median of an array of size 5 or less
int findMedianSelect(int arr[], int low, int high) {
    //int size = high - low + 1;
    insertionSort(arr, low, high);
    /*
    for (int i = 0; i < size - 1; i++) {
        for (int j = i + 1; j < size; j++) {
            comparisons++;
            if (arr[low + i] > arr[low + j]) {
                swap(&arr[low + i], &arr[low + j]);
            }
        }
    }
    */
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

// Function to partition the array around two pivots
void partitionDual(int array[], int low, int high, int* new_left, int* new_right) {
  int size = high - low + 1;
  int i = low + 1; 
  int from_left_index = low + 1;
  int from_right_index = high - 1;
  int d = 0;
  int left_pivot = array[low];
  int right_pivot = array[high];
  
  //int median = medianOfMediansSelect(array, low, high);
  int* tester_right = calloc(size, sizeof(int));
  int* tester_left = calloc(size, sizeof(int));
  tester_right = &array + low;
  tester_left = &array + low;
  //int tester_right[size];
  //int tester_left[size];
  //memcpy(tester_left, &array + low, size * sizeof(int));
  //memcpy(tester_right, &array + low, size * sizeof(int));
  
  int median_right = medianOfMediansSelect(tester_right, high / 2, high);
  int median_left = medianOfMediansSelect(tester_left, low, high / 2);
  free(tester_right);
  free(tester_left);

  if (median_left > median_right) {
    int temp = median_left;
    median_left = median_right;
    median_right = temp;
  }
  
  //if (left_pivot > median) {
    left_pivot = median_left;
    
    int r;
    for (r = low; r <= high; r++) {
        comparisons++;
        if (array[r] == left_pivot) {
            break;
        }
    }
    swap(&array[r], &array[low]);
    
    
  //}
  //else if (median > right_pivot) {
    right_pivot = median_right;
    
    //int r;
    for (r = low; r < high; r++) {
        comparisons++;
        if (array[r] == right_pivot) {
            break;
        }
    }
    swap(&array[r], &array[high]);
    
  //}
  
  //int left_pivot = array[low];
  //int right_pivot = array[high];
  
  while (from_left_index <= from_right_index) {
    if (d >= 0) {
    	comparisons++;
    	if (array[from_left_index] < left_pivot) {
    		swap(&array[from_left_index], &array[i]);
    		i++;
    		from_left_index++;
    		d++;
    	}
    	else {
    		comparisons++;
    		if (array[from_left_index] < right_pivot) {
    			from_left_index++;
    		}
    		else {
    			swap(&array[from_left_index], &array[from_right_index]);
    			from_right_index--;
    			d--;
    		}
    	}
    }
    else {
    	comparisons++;
    	if (array[from_right_index] > right_pivot) {
    		from_right_index--;
    		d--;
    	}
    	else {
    		comparisons++;
    		if (array[from_right_index] < left_pivot) {
    			rotate3(&array[from_right_index], &array[from_left_index], &array[i]);
    			i++;
    			d++;
    		}
    		else {
    			swap(&array[from_left_index], &array[from_right_index]);
    		}
    		from_left_index++;
    	}
    
    }
  }

  swap(&array[low], &array[i - 1]);
  swap(&array[high], &array[from_right_index + 1]);
    
  *new_left = i - 1;
  *new_right = from_right_index + 1;
}

void dualSort(int array[], int low, int high) {
  if (low < high) {
    comparisons++;
    if (array[low] > array[high]) {
    	swap(&array[low], &array[high]);
    }

    int right_new;
    int left_new;
    partitionDual(array, low, high, &left_new, &right_new);
    
    dualSort(array, low, left_new - 1);
    dualSort(array, left_new + 1, right_new - 1);
    dualSort(array, right_new + 1, high);
  }
}

int main() {
	int size;
	scanf("%d", &size);
	if (size < 50) {
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
	
	dualSort(input, 0, size - 1);
	
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