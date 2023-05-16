int comparisons = 0;
int swaps = 0;

void printArray(int arr[], int size) {
	for (int i = 0; i < size; i++) {
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

void rotate3(int *a, int *b, int *c) {
	int temp = *a;
	*a = *b;
	*b = *c;
	*c = temp;
	swaps += 3;
}

void swap(int *a, int *b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}
