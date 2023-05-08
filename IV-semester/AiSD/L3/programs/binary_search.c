#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "shared_functions.h"

int local_comparisons = 0;

// Recursive binary search function
int binarySearch(int arr[], int left, int right, int x) {
   if (right >= left) {
      int mid = left + (right - left) / 2;

      // If the element is present at the middle itself
      local_comparisons++;
      if (arr[mid] == x)
         return 1;

      local_comparisons++;
      // If element is smaller than mid, then it can only be present in left subarray
      if (arr[mid] > x)
         return binarySearch(arr, left, mid - 1, x);

      // Else the element can only be present in right subarray
      return binarySearch(arr, mid + 1, right, x);
   }

   // If element is not present in array
   return -1;
}

// Test the recursive binary search
int main() {
    srand(time(NULL));
    clock_t start, end;
    double cpu_time_used;
    int value;
    //Taking n
	scanf("%d", &global_size);
	int input[global_size];
	global_array = calloc(global_size, sizeof(int));
    //Taking value
    scanf("%d", &value);

    //Taking n keys
	for (int i = 0; i < global_size; i++) {
		scanf("%d", &input[i]);
		//global_array[i] = input[i];
	}
	
	if (global_size < 50) {
		printf("Given array:\n");
		printf("############################################################################################################################################\n");
		printArray(input, global_size);
		printf("############################################################################################################################################\n");
		printf("\n");
        printf("Value to be found: %d", value);
        printf("\n");
	}
	start = clock();
	int was_the_value_found = binarySearch(input, 0, global_size - 1, value);
	end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
	if (global_size < 50) {
        if (was_the_value_found > 0) {
            printf("The value was found!\n");
        }
        else {
            printf("The value was NOT found!\n");
        }
	}
	printf("Number of comparisons: %d\n", local_comparisons);
    printf("Time taken: %lf \u00B5s\n", cpu_time_used * 1000000);
	
	return 0;
}