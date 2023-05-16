#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "mtwister.h"

#define UPPER_MASK		0x80000000
#define LOWER_MASK		0x7fffffff
#define TEMPERING_MASK_B	0x9d2c5680
#define TEMPERING_MASK_C	0xefc60000

inline static void m_seedRand(MTRand* rand, unsigned long seed) {
  rand->mt[0] = seed & 0xffffffff;
  for(rand->index=1; rand->index<STATE_VECTOR_LENGTH; rand->index++) {
    rand->mt[rand->index] = (6069 * rand->mt[rand->index-1]) & 0xffffffff;
  }
}

MTRand seedRand(unsigned long seed) {
  MTRand rand;
  m_seedRand(&rand, seed);
  return rand;
}

unsigned long genRandLong(MTRand* rand) {
  unsigned long y;
  static unsigned long mag[2] = {0x0, 0x9908b0df};
  if(rand->index >= STATE_VECTOR_LENGTH || rand->index < 0) {
    int kk;
    if(rand->index >= STATE_VECTOR_LENGTH+1 || rand->index < 0) {
      m_seedRand(rand, 4357);
    }
    for(kk=0; kk<STATE_VECTOR_LENGTH-STATE_VECTOR_M; kk++) {
      y = (rand->mt[kk] & UPPER_MASK) | (rand->mt[kk+1] & LOWER_MASK);
      rand->mt[kk] = rand->mt[kk+STATE_VECTOR_M] ^ (y >> 1) ^ mag[y & 0x1];
    }
    for(; kk<STATE_VECTOR_LENGTH-1; kk++) {
      y = (rand->mt[kk] & UPPER_MASK) | (rand->mt[kk+1] & LOWER_MASK);
      rand->mt[kk] = rand->mt[kk+(STATE_VECTOR_M-STATE_VECTOR_LENGTH)] ^ (y >> 1) ^ mag[y & 0x1];
    }
    y = (rand->mt[STATE_VECTOR_LENGTH-1] & UPPER_MASK) | (rand->mt[0] & LOWER_MASK);
    rand->mt[STATE_VECTOR_LENGTH-1] = rand->mt[STATE_VECTOR_M-1] ^ (y >> 1) ^ mag[y & 0x1];
    rand->index = 0;
  }
  y = rand->mt[rand->index++];
  y ^= (y >> 11);
  y ^= (y << 7) & TEMPERING_MASK_B;
  y ^= (y << 15) & TEMPERING_MASK_C;
  y ^= (y >> 18);
  return y;
}

unsigned int genRand(MTRand* rand) {
  return((int)genRandLong(rand) / (int)0xffffffff);
}

int comparisons_quick = 0;
int swaps_quick = 0;
int comparisons_dual = 0;
int swaps_dual = 0;

void swap(int *a, int *b) {
  if (a == b) {
  	return;
  }
  int temp = *a;
  *a = *b;
  *b = temp;
}


void rotate3(int *a, int *b, int *c) {
	int temp = *a;
	*a = *b;
	*b = *c;
	*c = temp;
	swaps_dual += 3;
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

int partitionQuick(int array[], int low, int high) {
  int pivot = array[high];
  int i = (low - 1);

  for (int j = low; j < high; j++) {
    comparisons_quick++;
    if (array[j] <= pivot) {
        i++;
      	swap(&array[i], &array[j]);
        swaps_quick++;
      }
  }

  swap(&array[i + 1], &array[high]);
  swaps_quick++;
  
  return (i + 1);
}

void quickSort(int array[], int low, int high) {
  if (low < high) {
    int pi = partitionQuick(array, low, high);
    quickSort(array, low, pi - 1);
    quickSort(array, pi + 1, high);
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
    	comparisons_dual++;
    	if (array[from_left_index] < left_pivot) {
    		swap(&array[from_left_index], &array[i]);
            swaps_dual++;
    		i++;
    		from_left_index++;
    		d++;
    	}
    	else {
    		comparisons_dual++;
    		if (array[from_left_index] < right_pivot) {
    			from_left_index++;
    		}
    		else {
    			swap(&array[from_left_index], &array[from_right_index]);
                swaps_dual++;
    			from_right_index--;
    			d--;
    		}
    	}
    }
    else {
    	comparisons_dual++;
    	if (array[from_right_index] > right_pivot) {
    		from_right_index--;
    		d--;
    	}
    	else {
    		comparisons_dual++;
    		if (array[from_right_index] < left_pivot) {
    			rotate3(&array[from_right_index], &array[from_left_index], &array[i]);
    			i++;
    			d++;
    		}
    		else {
    			swap(&array[from_left_index], &array[from_right_index]);
                swaps_dual++;
    		}
    		from_left_index++;
    	}
    
    }
  }

  swap(&array[low], &array[i - 1]);
  swap(&array[high], &array[from_right_index + 1]);
  swaps_dual+=2;
    
  *new_left = i - 1;
  *new_right = from_right_index + 1;
}

void dualSort(int array[], int low, int high) {
  if (low < high) {
    comparisons_dual++;
    if (array[low] > array[high]) {
    	swap(&array[low], &array[high]);
        swaps_dual++;
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
    srand(time(NULL));
	char* filename;
	FILE* fp;
	filename = "dual_vs_quick.csv";
    fp = fopen(filename, "w");
    if (fp == NULL) {
        printf("Error opening the file %s", filename);
    	return -1;
    }
	fprintf(fp, "type;n;avg_comparisons;avg_swaps;constant;\n");
    for (int n = 1000; n <= 50000; n += 1000) {
        printf("##### TEST %d #####\n", n);
        double average_comparisons_quick = 0.0;
        double average_comparisons_dual = 0.0;
        double average_swaps_quick = 0.0;
        double average_swaps_dual = 0.0;
        double constant_quick = 0.0;
        double constant_dual = 0.0;
        for (int k = 1; k <= 50; k++) {
            comparisons_quick = 0;
            comparisons_dual = 0;
            swaps_quick = 0;
            swaps_dual = 0;
            int quick[n];
            int dual[n];
            MTRand r = seedRand(rand());
            for(int i=0; i < n; i++) {
  			    quick[i] = genRand(&r) % (2*n);
                dual[i] = quick[i];
  		    }
            quickSort(quick, 0, n - 1);
            dualSort(dual, 0, n - 1);
            average_comparisons_quick += comparisons_quick;
            average_comparisons_dual += comparisons_dual;
            average_swaps_quick += swaps_quick;
            average_swaps_dual += swaps_dual;
        }
        average_comparisons_quick /= 50.0;
        average_comparisons_dual /= 50.0;
        average_swaps_quick /= 50.0;
        average_swaps_dual /= 50.0;
        constant_quick = average_comparisons_quick / (n * log((double)n));
        constant_dual = average_comparisons_dual / (n * log((double)n));
        fprintf(fp, "quick;%d;%f;%f;%f;\n", n, average_comparisons_quick, average_swaps_quick, constant_quick);
        fprintf(fp, "dual;%d;%f;%f;%f;\n", n, average_comparisons_dual, average_swaps_dual, constant_dual);
    }
    fclose(fp);
	return 0;
}