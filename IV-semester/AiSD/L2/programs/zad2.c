#include <stdio.h>
#include <stdbool.h>
#include "algorithms.h"

#define UPPER_MASK		0x80000000
#define LOWER_MASK		0x7fffffff
#define TEMPERING_MASK_B	0x9d2c5680
#define TEMPERING_MASK_C	0xefc60000

#include "mtwister.h"
#include <stdlib.h>
#include <time.h>

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


int main() {
	srand(time(NULL));
	char* filename;
	FILE* fp;
	filename = "data.csv";
	fp = fopen(filename, "w");
	if (fp == NULL) {
        	printf("Error opening the file %s", filename);
        	return -1;
    }
    fprintf(fp, "type;n;k;comps;swaps;\n");

	for (int n = 10; n <= 200; n+=10) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		insertionSort(arr, n);
  		fprintf(fp, "insertion;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			insertionSort(arr, n);
  			fprintf(fp, "insertion;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			insertionSort(arr, n);
  			fprintf(fp, "insertion;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	printf("INSERTION_SORT - DONE\n");
	for (int n = 10; n <= 200; n+=10) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		mergeSort(arr, 0, n - 1);
  		fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);

	}

	for (int n = 1000; n <= 20000; n+=1000) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		mergeSort(arr, 0, n - 1);
  		fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			mergeSort(arr, 0, n - 1);
  			fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			mergeSort(arr, 0, n - 1);
  			fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			mergeSort(arr, 0, n - 1);
  			fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			mergeSort(arr, 0, n - 1);
  			fprintf(fp, "merge;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	printf("MERGE_SORT - DONE\n");

	for (int n = 10; n <= 200; n+=10) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		quickSort(arr, 0, n - 1);
  		fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		quickSort(arr, 0, n - 1);
  		fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			quickSort(arr, 0, n - 1);
  			fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			quickSort(arr, 0, n - 1);
  			fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}

	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			quickSort(arr, 0, n - 1);
  			fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			quickSort(arr, 0, n - 1);
  			fprintf(fp, "quick;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	printf("QUICK_SORT - DONE\n");
	
	for (int n = 10; n <= 200; n+=10) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		dualSort(arr, 0, n - 1);
  		fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		dualSort(arr, 0, n - 1);
  		fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}
	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			dualSort(arr, 0, n - 1);
  			fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}
	
	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			dualSort(arr, 0, n - 1);
  			fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}
	
	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			dualSort(arr, 0, n - 1);
  			fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			dualSort(arr, 0, n - 1);
  			fprintf(fp, "dual;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	printf("DUAL-PIVOT_QUICK_SORT - DONE\n");

	for (int n = 10; n <= 200; n+=10) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		hybridSort(arr, 0, n - 1);
  		fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}

	for (int n = 1000; n <= 20000; n+=1000) {
		comparisons = 0;
		swaps = 0;
		int arr[n];
		MTRand r = seedRand(rand());
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
  		}
  		hybridSort(arr, 0, n - 1);
  		fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 1, comparisons, swaps);
	}
	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			hybridSort(arr, 0, n - 1);
  			fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}
	
	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 10; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			hybridSort(arr, 0, n - 1);
  			fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 10, comparisons, swaps);
		}
	}
	
	for (int n = 10; n <= 200; n+=10) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			hybridSort(arr, 0, n - 1);
  			fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	for (int n = 1000; n <= 20000; n+=1000) {
		for (int k = 1; k <= 100; k++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			MTRand r = seedRand(rand());
  			for(int i=0; i < n; i++) {
  				arr[i] = genRand(&r) % (2*n);
  			}
  			hybridSort(arr, 0, n - 1);
  			fprintf(fp, "hybrid;%d;%d;%d;%d;\n", n, 100, comparisons, swaps);
		}
	}
	printf("HYBRID_SORT - DONE\n");
	
	fclose(fp);
	
	return 0;
}
