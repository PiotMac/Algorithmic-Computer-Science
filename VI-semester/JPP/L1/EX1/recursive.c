#include "library.h"

uint64_t factorial(uint64_t n) {
    if (n == 0) {
        return 1;
    } 
    else {
        return n * factorial(n - 1);
    }
}

uint64_t gcd(uint64_t a, uint64_t b) {
    if (b == 0) {
        return a;
    } 
    else {
        return gcd(b, a % b);
    }
}

Solution diophantine_equation(int64_t a, int64_t b, int64_t c) {
    Solution solution = {0, 0};

    if (c == 0) {
        return solution;
    } else if (a == 0 && b == 0) {
        return solution;
    } else if (a == 0) {
        solution.y = c / b;
        return solution;
    } else if (b == 0) {
        solution.x = c / a;
        return solution;
    } else {
        Solution tmp = diophantine_equation(b, a % b, c);
        solution.x = tmp.y;
        solution.y = tmp.x - (a / b) * tmp.y;
        return solution;
    }
}