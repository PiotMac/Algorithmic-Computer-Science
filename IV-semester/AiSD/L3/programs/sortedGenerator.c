#define UPPER_MASK		0x80000000
#define LOWER_MASK		0x7fffffff
#define TEMPERING_MASK_B	0x9d2c5680
#define TEMPERING_MASK_C	0xefc60000

#include "mtwister.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

inline static void m_seedRand(MTRand* rand, unsigned long seed) {
  /* set initial seeds to mt[STATE_VECTOR_LENGTH] using the generator
   * from Line 25 of Table 1 in: Donald Knuth, "The Art of Computer
   * Programming," Vol. 2 (2nd Ed.) pp.102.
   */
  rand->mt[0] = seed & 0xffffffff;
  for(rand->index=1; rand->index<STATE_VECTOR_LENGTH; rand->index++) {
    rand->mt[rand->index] = (6069 * rand->mt[rand->index-1]) & 0xffffffff;
  }
}

/**
* Creates a new random number generator from a given seed.
*/
MTRand seedRand(unsigned long seed) {
  MTRand rand;
  m_seedRand(&rand, seed);
  return rand;
}

/**
 * Generates a pseudo-randomly generated long.
 */
unsigned long genRandLong(MTRand* rand) {

  unsigned long y;
  static unsigned long mag[2] = {0x0, 0x9908b0df}; /* mag[x] = x * 0x9908b0df for x = 0,1 */
  if(rand->index >= STATE_VECTOR_LENGTH || rand->index < 0) {
    /* generate STATE_VECTOR_LENGTH words at a time */
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

/**
 * Generates a pseudo-randomly generated double in the range [0..1].
 */
unsigned int genRand(MTRand* rand) {
  return((int)genRandLong(rand) / (int)0xffffffff);
}

void printArray(int arr[], int n) {
	for (int i = 0; i < n; i++) {
		printf("%d ", arr[i]);
	}
	printf("\n");
}

int main(int argc, char* argv[]) {
	srand(time(NULL));
	int n = atoi(argv[1]);
	int randoms[n];
	MTRand r = seedRand(rand());
    //randoms[0] = genRand(&r) % (10*n);
    //for the beginning of the array
    //int searched_value = genRand(&r) % (n / 10);
    //for the middle of the array
    //int searched_value = 4 * (n / 10) + genRand(&r) % (2 * n / 10);
    //for the end of the array
    //int searched_value = (genRand(&r) % (n / 10)) + 9 * (n / 10);
    //random value in the scope of an array
    int searched_value = genRand(&r) % n;
  	for(int i=0; i < n; i++) {
        /*
        int next_value = genRand(&r) % (10*n);
        while (randoms[i] > next_value) {
            next_value = genRand(&r) % (10*n);
        }
  		randoms[i + 1] = next_value;
        */
        //randoms[i] = randoms[i - 1] + genRand(&r) % 10;
        randoms[i] = i;
  	}
  	printf("%d ", n);
    printf("%d ", searched_value);
  	printArray(randoms, n);
  	return 0;
}
