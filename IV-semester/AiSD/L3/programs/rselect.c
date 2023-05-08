#include "shared_functions.h"
//#include "algorithms.h"
#include <time.h>
#include <stdlib.h>

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

int randomInt(int lower_bound, int higher_bound) {
    return (rand() % (higher_bound - lower_bound + 1)) + lower_bound;
}

int partition(int arr[], int low, int high) {
  int pivot = arr[high];
  int i = (low - 1);

  for (int j = low; j < high; j++) {
    comparisons++;
    if (arr[j] <= pivot) {
        i++;
      	swap(&arr[i], &arr[j]);
      }
  }
  swap(&arr[i + 1], &arr[high]);
  
  return (i + 1);
}

int randomPartition(int arr[], int p, int q) {
    int pivot_index = randomInt(p, q);
    swap(&arr[pivot_index], &arr[q]);
    return partition(arr, p, q);
}

int randomSelect(int arr[], int p, int q, int position) {
    if (p == q) {
        return arr[p];
    }
    int random_index = randomPartition(arr, p, q);
    int local_index = random_index - p + 1;
    if (global_size < 50) {
        printArray(arr, global_size);
    }
    if (local_index == position) {
        return arr[random_index];
    }
    else if (local_index < position) {
        return randomSelect(arr, random_index + 1, q, position - local_index);
    }
    else {
        return randomSelect(arr, p, random_index - 1, position);
    }
}

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
	
	int found_statistic = randomSelect(input, 0, global_size - 1, position);
    int local_comparisons = comparisons;
    int local_swaps = swaps;
	
	if (global_size < 50) {
        printf("\n");
        printf("Array after Randomized Select:\n");
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
	printf("Number of comparisons: %d\n", local_comparisons);
	printf("Number of swaps: %d\n", local_swaps);
	
	return 0;
}
