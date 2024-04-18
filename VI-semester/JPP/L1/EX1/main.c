#include <stdio.h>
#include "library.h"

int main() {
    printf("12! = %lu\n", factorial(12));
    printf("GCD(893724,2947228) = %lu\n", gcd(893724, 2947228));
    int64_t a = 2718;
    int64_t b = 3872;
    int64_t c = 2;
    printf("Finding solution for: %ldx + %ldy = %ld\n", a, b, c);
    Solution solution = diophantine_equation(a, b, c);
    if (solution.x == 0 && solution.y == 0 && c != 0) {
        printf("No integer solutions!\n");
    }
    else {
        printf("x = %ld, y = %ld\n", solution.x, solution.y);
    }

    return 0;
}