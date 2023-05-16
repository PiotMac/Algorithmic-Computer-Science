#include "common.h"

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

void insertionSort2(int arr[], int low, int high) {
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
	}
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
}

void mergeSort(int arr[], int left, int right) {
	if (left < right) {
		int mid = left + (right - left) / 2;
		mergeSort(arr, left, mid);
        	mergeSort(arr, mid + 1, right);
        	merge(arr, left, mid, right);
	}
}

int partitionQuick(int array[], int low, int high) {
  int pivot = array[high];

  int i = (low - 1);
  for (int j = low; j < high; j++) {
    comparisons++;
    if (array[j] <= pivot) {
      i++;
      	swap(&array[i], &array[j]);
      	swaps++;
    }
  }
	swap(&array[i + 1], &array[high]);
  	swaps++;
  return (i + 1);
}

void quickSort(int array[], int low, int high) {
  if (low < high) {
    int pivot = partitionQuick(array, low, high);

    quickSort(array, low, pivot - 1);
    quickSort(array, pivot + 1, high);
  }
}

void partitionDual(int array[], int low, int high, int* new_left, int* new_right) {
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
    		swaps++;
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
    			swaps++;
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
    			swaps++;
    		}
    		from_left_index++;
    	}
    
    }
  }
    swap(&array[low], &array[i - 1]);
    swaps++;
  	swap(&array[high], &array[from_right_index + 1]);
  	swaps++;
    
  *new_left = i - 1;
  *new_right = from_right_index + 1;
}

void dualSort(int array[], int low, int high) {
  if (low < high) {
    comparisons++;
    if (array[low] > array[high]) {
    	swap(&array[low], &array[high]);
		swaps++;
    }
    
    
    int right_new;
    int left_new;
    partitionDual(array, low, high, &left_new, &right_new);
    
    dualSort(array, low, left_new - 1);
    dualSort(array, left_new + 1, right_new - 1);
    dualSort(array, right_new + 1, high);
  }
}

void hybridSort(int array[], int low, int high) {
    if (low < high) {
        if (high - low <= 5) {
            insertionSort2(array, low, high);
        }
        else {
            int pivot = partitionQuick(array, low, high);
            hybridSort(array, low, pivot - 1);
            hybridSort(array, pivot + 1, high);
        }
    }
}



