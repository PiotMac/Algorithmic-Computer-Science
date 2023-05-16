#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

int comparisons = 0;
int swaps = 0;

int* global_array;
int global_size;
bool print = false;

void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++) {
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

void merge(int arr[], int left, int mid, int right) {
	int first = mid - left + 1;
	int second = right - mid;
	
	int L[first], R[second];
	
	for (int i = 0; i < first; i++) {
		L[i] = arr[left + i];
	}
	for (int i = 0; i < second; i++) {
		R[i] = arr[mid + i + 1];
	}
	
	int k = left;
	int i = 0;
	int j = 0;
	while (i < first && j < second) 
    	{	
    		comparisons++;
        	if (L[i] <= R[j]) 
        	{
            		arr[k] = L[i];
            		i++;
        	}
        	else 
        	{
            		arr[k] = R[j];
            		j++;
            		swaps++;
        	}
        	k++;
    	}
    	
    	while (i < first) {
        	arr[k] = L[i];
        	i++;
        	k++;
    	}
    	
    	while (j < second) 
    	{
        	arr[k] = R[j];
        	j++;
        	k++;
    	}
    	
	if (print) {
    	for (int i = 0; i < global_size; i++) {
    		if (global_array[i] != arr[i]) {
    			global_array[i] = arr[i];
    		}
    	}
    	printArray(global_array, global_size);
    }
}

void mergeSort(int arr[], int left, int right) {
	if (left < right) {
		int mid = left + (right - left) / 2;
		mergeSort(arr, left, mid);
        	mergeSort(arr, mid + 1, right);
        	merge(arr, left, mid, right);
	}
}

int main()
{
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
    		printf("NOT sorted array is:\n");
    		printf("############################################################################################################################################\n");
    		printArray(input, size);
    		printf("############################################################################################################################################\n");
    		printf("\n");	
    	}
    	mergeSort(input, 0, size - 1);
    	if (print) {
    		printf("\n");
    		printf("Sorted array is:\n");
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
