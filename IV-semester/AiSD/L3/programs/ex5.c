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
    clock_t start, end;
    double cpu_time_used;
    double avg_comps_quickSelect;
    double avg_comps_quick;
    double avg_swaps_quickSelect;
    double avg_swaps_quick;
    double avg_time_quickSelect;
    double avg_time_quick;
    double avg_worst_comps_quickSelect;
    double avg_worst_comps_quick;
    double avg_worst_swaps_quickSelect;
    double avg_worst_swaps_quick;
    double avg_worst_time_quickSelect;
    double avg_worst_time_quick;
    
	srand(time(NULL));
	char* filename;
	FILE* fp;
	filename = "ex5.csv";
	fp = fopen(filename, "w");
	if (fp == NULL) {
        	printf("Error opening the file %s", filename);
        	return -1;
    }
    fprintf(fp, "type;data;n;time;comps;swaps\n");

	for (int n = 100; n <= 10000; n+=100) {
		printf("##### TEST %d #####\n", n);
        avg_comps_quickSelect = 0.0;
        avg_comps_quick = 0.0;
        avg_swaps_quickSelect = 0.0;
        avg_swaps_quick = 0.0;
        avg_time_quickSelect = 0.0;
        avg_time_quick = 0.0;
        avg_worst_comps_quickSelect = 0.0;
        avg_worst_comps_quick = 0.0;
        avg_worst_swaps_quickSelect = 0.0;
        avg_worst_swaps_quick = 0.0;
        avg_worst_time_quickSelect = 0.0;
        avg_worst_time_quick = 0.0;
		for (int m = 1; m <= 100; m++) {
            MTRand r = seedRand(rand());
			comparisons = 0;
			swaps = 0;
			int test1[n];
			int test2[n];
            int worst1[n];
            int worst2[n];
            for(int i=0; i < n; i++) {
  			    test1[i] = genRand(&r) % (2*n);
                test2[i] = test1[i];
  		    }
            worst1[0] = 0;
            worst2[0] = 0;
  			for(int i=1; i < n; i++) {
				worst1[i] = 2 * i - 1;
				worst2[i] = worst1[i];
  			}
            start = clock();
            quickSortSelect(test1, 0, n - 1);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_quickSelect += cpu_time_used;
            avg_comps_quickSelect += comparisons;
            avg_swaps_quickSelect += swaps;
            comparisons = 0;
            swaps = 0;
            start = clock();
            quickSort(test2, 0, n - 1);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_time_quick += cpu_time_used;
            avg_comps_quick += comparisons;
            avg_swaps_quick += swaps;
            comparisons = 0;
            swaps = 0;
            start = clock();
            quickSortSelect(worst1, 0, n - 1);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_worst_time_quickSelect += cpu_time_used;
            avg_worst_comps_quickSelect += comparisons;
            avg_worst_swaps_quickSelect += swaps;
            comparisons = 0;
            swaps = 0;
            start = clock();
            quickSort(worst2, 0, n - 1);
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            avg_worst_time_quick += cpu_time_used;
            avg_worst_comps_quick += comparisons;
            avg_worst_swaps_quick += swaps;
        }
        fprintf(fp, "quickSortSelect;random;%d;%lf;%lf;%lf\n", n, (avg_time_quickSelect / 100.0) * 1000000, avg_comps_quickSelect / 100.0, avg_swaps_quickSelect / 100.0);
        fprintf(fp, "quickSort;random;%d;%lf;%lf;%lf\n", n, (avg_time_quick / 100.0) * 1000000, avg_comps_quick / 100.0, avg_swaps_quick / 100.0);
        fprintf(fp, "quickSortSelect;worst;%d;%lf;%lf;%lf\n", n, (avg_worst_time_quickSelect / 100.0) * 1000000, avg_worst_comps_quickSelect / 100.0, avg_worst_swaps_quickSelect / 100.0);
        fprintf(fp, "quickSort;worst;%d;%lf;%lf;%lf\n", n, (avg_worst_time_quick / 100.0) * 1000000, avg_worst_comps_quick / 100.0, avg_worst_swaps_quick / 100.0);
	}
	fclose(fp);
	
	return 0;
}
