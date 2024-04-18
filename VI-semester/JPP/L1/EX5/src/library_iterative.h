#ifndef ILIBRARY_H
#define ILIBRARY_H

#include <stdint.h>

typedef struct {
    int64_t x;
    int64_t y;
} Solution;

uint64_t factorial_iterative(uint64_t n);
uint64_t gcd_iterative(uint64_t a, uint64_t b);
Solution diophantine_equation_iterative(int64_t a, int64_t b, int64_t c);

#endif