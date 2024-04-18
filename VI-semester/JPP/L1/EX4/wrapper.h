#ifndef WRAPPER_H
#define WRAPPER_H

#include <stdint.h>

typedef struct {
    int64_t x;
    int64_t y;
} Solution;

extern uint64_t factorial_iterative(uint64_t n);
extern uint64_t gcd_iterative(uint64_t a, uint64_t b);
extern Solution diophantine_equation_iterative(int64_t a, int64_t b, int64_t c);

extern uint64_t factorial_recursive(uint64_t n);
extern uint64_t gcd_recursive(uint64_t a, uint64_t b);
extern Solution diophantine_equation_recursive(int64_t a, int64_t b, int64_t c);

#endif