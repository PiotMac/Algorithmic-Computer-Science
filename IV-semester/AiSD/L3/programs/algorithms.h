#include "shared_functions.h"
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

int rPartition(int arr[], int low, int high) {
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
    return rPartition(arr, p, q);
}

int randomSelect(int arr[], int p, int q, int position) {
    if (p == q) {
        return arr[p];
    }
    int random_index = randomPartition(arr, p, q);
    int local_index = random_index - p + 1;

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

int findMedian(int arr[], int size) {
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

int medianOfMedians(int arr[], int n) {
    if (n <= 5) {
        return findMedian(arr, n);
    }
    int numGroups = (n + 4) / 5;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * 5;
        int groupEnd = (groupStart + 4 < n) ? (groupStart + 4) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedian(arr + groupStart, groupSize);
    }
    return medianOfMedians(medians, numGroups);
}

int partitionSel(int arr[], int n, int pivot) {
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

int selectAlg(int arr[], int n, int k) {
    if (n == 1) {
        return arr[0];
    }
    int pivot = medianOfMedians(arr, n);
    int pivotIndex = partitionSel(arr, n, pivot);

    if (k == pivotIndex) {
        return arr[pivotIndex];
    } else if (k < pivotIndex) {
        return selectAlg(arr, pivotIndex, k);
    } else {
        return selectAlg(arr + pivotIndex + 1, n - pivotIndex - 1, k - pivotIndex - 1);
    }
}

int medianOfMediansExp(int arr[], int n, int divider) {
    if (n <= divider) {
        return findMedian(arr, n);
    }
    int numGroups = (n + (divider - 1)) / divider;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * divider;
        int groupEnd = (groupStart + (divider - 1) < n) ? (groupStart + (divider - 1)) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedian(arr + groupStart, groupSize);
    }
    return medianOfMediansExp(medians, numGroups, divider);
}

int selectExperimental(int arr[], int n, int k, int divider) {
    if (n == 1) {
        return arr[0];
    }
    int pivot = medianOfMediansExp(arr, n, divider);
    int pivotIndex = partitionSel(arr, n, pivot);

    if (k == pivotIndex) {
        return arr[pivotIndex];
    } else if (k < pivotIndex) {
        return selectExperimental(arr, pivotIndex, k, divider);
    } else {
        return selectExperimental(arr + pivotIndex + 1, n - pivotIndex - 1, k - pivotIndex - 1, divider);
    }
}

// Recursive binary search function
int binarySearch(int arr[], int left, int right, int x) {
   if (right >= left) {
      int mid = left + (right - left) / 2;

      // If the element is present at the middle itself
      comparisons++;
      if (arr[mid] == x)
         return 1;

      comparisons++;
      // If element is smaller than mid, then it can only be present in left subarray
      if (arr[mid] > x)
         return binarySearch(arr, left, mid - 1, x);

      // Else the element can only be present in right subarray
      return binarySearch(arr, mid + 1, right, x);
   }

   // If element is not present in array
   return -1;
}

int findMedianQuickSelect(int arr[], int low, int high) {
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

int medianOfMediansQuickSelect(int arr[], int low, int high) {
    int n = high - low + 1;
    if (n <= 5) {
        return findMedianQuickSelect(arr, low, high);
    }
    int numGroups = (n + 4) / 5;
    int medians[numGroups];
    for (int i = 0; i < numGroups; i++) {
        int groupStart = i * 5;
        int groupEnd = (groupStart + 4 < n) ? (groupStart + 4) : (n - 1);
        int groupSize = groupEnd - groupStart + 1;
        medians[i] = findMedianQuickSelect(arr, low + groupStart, low + groupStart + groupSize - 1);
    }
    //printArray(medians, numGroups);
    return medianOfMediansQuickSelect(medians, 0, numGroups - 1);
}

int partitionQuickSelect(int arr[], int low, int high, int pivot) {
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

void quickSortSelect(int array[], int low, int high) {
  if (low < high) {
    int median_of_medians_pivot = medianOfMediansQuickSelect(array, low, high);
    int pi = partitionQuickSelect(array, low, high, median_of_medians_pivot);
    quickSortSelect(array, low, pi - 1);
    quickSortSelect(array, pi + 1, high);
  }
}

void quickSort(int array[], int low, int high) {
  if (low < high) {
    int pi = rPartition(array, low, high);
    quickSort(array, low, pi - 1);
    quickSort(array, pi + 1, high);
  }
}
