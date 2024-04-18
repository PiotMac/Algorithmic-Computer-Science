#include <stdio.h>
#include "library.h"

int main() {
    printf("12! = %ld\n", factorial(12));
    printf("GCD(893724,2947228) = %ld\n", gcd(893724,2947228));
    Solution solution = diophantine_equation(2718, 3872, 2);
    printf("Solution to the diophantine equation '2718x + 3872y = 2' : x = %ld, y = %ld\n", solution.x, solution.y);
    
    return 0;
}