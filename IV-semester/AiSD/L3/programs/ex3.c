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
	filename = "ex3.csv";
	fp = fopen(filename, "w");
	if (fp == NULL) {
        	printf("Error opening the file %s", filename);
        	return -1;
    }
  fprintf(fp, "type;n;k;comps;swaps\n");
  MTRand r = seedRand(rand());
	for (int n = 100; n <= 10000; n+=100) {
		int position = (genRand(&r) % n) + 1;
		printf("Próba dla %d\n", n);
		for (int m = 1; m <= 100; m++) {
			comparisons = 0;
			swaps = 0;
			int arr[n];
			int test1[n];
			int test2[n];
      int test3[n];
			int test4[n];
  		for(int i=0; i < n; i++) {
  			arr[i] = genRand(&r) % (2*n);
	  		test1[i] = arr[i];
				test2[i] = arr[i];
        test3[i] = arr[i];
				test4[i] = arr[i];
  		}
		
			selectExperimental(test1, n, position - 1, 3);
			fprintf(fp, "ExperimentalSelect3;%d;%d;%d;%d\n", n, 3, comparisons, swaps);
			comparisons = 0;
			swaps = 0;
			selectExperimental(test2, n, position - 1, 5);
			fprintf(fp, "ExperimentalSelect5;%d;%d;%d;%d\n", n, 5, comparisons, swaps);
      comparisons = 0;
			swaps = 0;
      selectExperimental(test3, n, position - 1, 7);
			fprintf(fp, "ExperimentalSelect7;%d;%d;%d;%d\n", n, 7, comparisons, swaps);
      comparisons = 0;
			swaps = 0;
      selectExperimental(test4, n, position - 1, 9);
			fprintf(fp, "ExperimentalSelect9;%d;%d;%d;%d\n", n, 9, comparisons, swaps);
		}
	}
	fclose(fp);
	
	return 0;
}