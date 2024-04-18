#include "library.h"

uint64_t factorial(uint64_t n) {
    uint64_t result = 1;

    for (uint64_t i = 2; i <= n; i++) {
        result *= i;
    }

    return result;
}

uint64_t gcd(uint64_t a, uint64_t b) {
    while (b != 0) {
        uint64_t temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

Solution diophantine_equation(int64_t a, int64_t b, int64_t c) {
    Solution solution = {0, 0};

    int64_t old_r = a, r = b;
    int64_t old_s = 1, s = 0;
    int64_t old_t = 0, t = 1;

    while (r != 0) {
        int64_t quotient = old_r / r;
        int64_t temp = old_r;

        old_r = r;
        r = temp - quotient * r;

        temp = old_s;
        old_s = s;
        s = temp - quotient * s;

        temp = old_t;
        old_t = t;
        t = temp - quotient * t;
    }

    if (c % old_r == 0) {
        int64_t k = c / old_r;
        solution.x = old_s * k;
        solution.y = old_t * k;
    }

    return solution;
}