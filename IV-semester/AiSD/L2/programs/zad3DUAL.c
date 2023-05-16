#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

int comparisons = 0;
int swaps = 0;

int global_size = 0;
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


void rotate3(int *a, int *b, int *c) {
	int temp = *a;
	*a = *b;
	*b = *c;
	*c = temp;
	swaps += 3;
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

void partition(int array[], int low, int high, int* new_left, int* new_right) {
  int i = low + 1; 
  int from_left_index = low + 1;
  int from_right_index = high - 1;
  int d = 0;
  int left_pivot = array[low];
  int right_pivot = array[high];
  
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
  
  if (print) {
    	for (int i = 0; i < global_size; i++) {
    		if (global_array[i] != array[i]) {
    			global_array[i] = array[i];
    		}
    	}
    	printArray(global_array, global_size);
  }
}

void dualSort(int array[], int low, int high) {
  if (low < high) {
    comparisons++;
    if (array[low] > array[high]) {
    	swap(&array[low], &array[high]);
    }
    
    
    int right_new;
    int left_new;
    partition(array, low, high, &left_new, &right_new);
    
    dualSort(array, low, left_new - 1);
    dualSort(array, left_new + 1, right_new - 1);
    dualSort(array, right_new + 1, high);
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
		printf("NOT sorted array:\n");		printf("############################################################################################################################################\n");
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
	double constant = comparisons / (size * log((double)size));
	printf("Constant: %f\n", constant);
	
	if (isSorted(input, size) == true) {
		printf("SORTING SUCCESSFUL!\n");
	}
	else {
		printf("Sorting NOT successful!\n");
	}
	
	return 0;
}
