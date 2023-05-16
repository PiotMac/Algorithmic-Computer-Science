#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
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
int comparisons_insertion = 0;
int swaps_quick = 0;
int swaps_insertion = 0;
int counter = 0;

void swap(int *a, int *b) {
  if (a == b) {
  	return;
  }
  int temp = *a;
  *a = *b;
  *b = temp;
}

void insertionSort(int arr[], int low, int high) {
	int key, j;
	for (int i = low + 1; i <= high; i++) {
		key = arr[i];
		j = i;
		comparisons_insertion++;
		while (j > low && arr[j - 1] > key) {
			arr[j] = arr[j - 1];
			j = j - 1;
			swaps_insertion++;
			comparisons_insertion++;
		}
		arr[j] = key;
	}
}

int partition(int array[], int low, int high) {
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
    int pi = partition(array, low, high);
    quickSort(array, low, pi - 1);
    quickSort(array, pi + 1, high);
  }
}

void hybridSort(int array[], int low, int high) {
    if (low < high) {
        if (high - low <= 8) {
            insertionSort(array, low, high);
        }
        else {
            int pivot = partition(array, low, high);
            hybridSort(array, low, pivot - 1);
            hybridSort(array, pivot + 1, high);
        }
    }
}

int main() {
	srand(time(NULL));
	char* filename;
	FILE* fp;
	filename = "hybrid_all8.csv";
    fp = fopen(filename, "w");
    if (fp == NULL) {
        printf("Error opening the file %s", filename);
    	return -1;
    }
	fprintf(fp, "type;n;avg_comparisons;avg_swaps;\n");
	
    for (int n = 10; n <= 2000; n += 10) {
        printf("##### TEST %d #####\n", n);
        double average_comparisons_quick = 0.0;
        double average_comparisons_hybrid = 0.0;
        double average_comparisons_insertion = 0.0;
        double average_swaps_quick = 0.0;
        double average_swaps_hybrid = 0.0;
        double average_swaps_insertion = 0.0;
        for (int k = 1; k <= 50; k++) {
            comparisons_quick = 0;
            comparisons_insertion = 0;
            swaps_quick = 0;
            swaps_insertion = 0;
            int quick[n];
            int hybrid[n];
            int insertion[n];
            MTRand r = seedRand(rand());
            for(int i=0; i < n; i++) {
  			    quick[i] = genRand(&r) % (2*n);
                hybrid[i] = quick[i];
                insertion[i] = quick[i];
  		    }
            quickSort(quick, 0, n - 1);
            insertionSort(insertion, 0, n - 1);
            average_comparisons_quick += comparisons_quick;
            average_comparisons_insertion += comparisons_insertion;
            average_swaps_quick += swaps_quick;
            average_swaps_insertion += swaps_insertion;
            comparisons_insertion = 0;
            comparisons_quick = 0;
            swaps_quick = 0;
            swaps_insertion = 0;
            hybridSort(hybrid, 0, n - 1);
            average_comparisons_hybrid = average_comparisons_hybrid + comparisons_quick + comparisons_insertion;
            average_swaps_hybrid = average_swaps_hybrid + swaps_quick + swaps_insertion;
        }
        average_comparisons_quick /= 50.0;
        average_comparisons_insertion /= 50.0;
        average_comparisons_hybrid /= 50.0;
        average_swaps_quick /= 50.0;
        average_swaps_insertion /= 50.0;
        average_swaps_hybrid /= 50.0;
        fprintf(fp, "quick;%d;%f;%f;\n", n, average_comparisons_quick, average_swaps_quick);
        fprintf(fp, "insertion;%d;%f;%f;\n", n, average_comparisons_insertion, average_swaps_insertion);
        fprintf(fp, "hybrid;%d;%f;%f;\n", n, average_comparisons_hybrid, average_swaps_hybrid);

    }
    fclose(fp);

    filename = "hybrid_not_all8.csv";
    fp = fopen(filename, "w");
    if (fp == NULL) {
        printf("Error opening the file %s", filename);
    	return -1;
    }
	fprintf(fp, "type;n;avg_comparisons;avg_swaps;\n");

    for (int n = 1000; n <= 50000; n += 1000) {
        printf("##### TEST %d #####\n", n);
        double average_comparisons_quick = 0.0;
        double average_comparisons_hybrid = 0.0;
        double average_swaps_quick = 0.0;
        double average_swaps_hybrid = 0.0;
        for (int k = 1; k <= 50; k++) {
            comparisons_quick = 0;
            comparisons_insertion = 0;
            swaps_quick = 0;
            swaps_insertion = 0;
            int quick[n];
            int hybrid[n];
            MTRand r = seedRand(rand());
            for(int i=0; i < n; i++) {
  			    quick[i] = genRand(&r) % (2*n);
                hybrid[i] = quick[i];
  		    }
            quickSort(quick, 0, n - 1);
            average_comparisons_quick += comparisons_quick;
            average_swaps_quick += swaps_quick;
            comparisons_insertion = 0;
            comparisons_quick = 0;
            swaps_quick = 0;
            swaps_insertion = 0;
            hybridSort(hybrid, 0, n - 1);
            average_comparisons_hybrid = average_comparisons_hybrid + comparisons_quick + comparisons_insertion;
            average_swaps_hybrid = average_swaps_hybrid + swaps_quick + swaps_insertion;
        }
        average_comparisons_quick /= 50.0;
        average_comparisons_hybrid /= 50.0;
        average_swaps_quick /= 50.0;
        average_swaps_hybrid /= 50.0;
        fprintf(fp, "quick;%d;%f;%f;\n", n, average_comparisons_quick, average_swaps_quick);
        fprintf(fp, "hybrid;%d;%f;%f;\n", n, average_comparisons_hybrid, average_swaps_hybrid);
    }    

    fclose(fp);
	return 0;
}