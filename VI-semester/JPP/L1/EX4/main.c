#include "wrapper.h"
#include <stdio.h>

int main()
{
	printf("##### ITERATIVE #####\n");
	printf("Factorial of 12: %ld\n", factorial_iterative(12));
	printf("Correct: 479001600\n");

	printf("GCD(893724,2947228) = %ld\n", gcd_iterative(893724,2947228));
	printf("Correct: 4\n");

	Solution solution = diophantine_equation_iterative(2718, 3872, 2);
	printf("Diophantine solution for 2718x + 3872y = 2: x = %ld, y = %ld\n", solution.x, solution.y);
	printf("Correct: x = 255, y = -179\n");

	printf("##### RECURSIVE #####\n");
	printf("Factorial of 12: %ld\n", factorial_recursive(12));
	printf("Correct: 479001600\n");

	printf("GCD(893724,2947228) = %ld\n", gcd_recursive(893724,2947228));
	printf("Correct: 4\n");

	solution = diophantine_equation_recursive(2718, 3872, 2);
	printf("Diophantine solution for 2718x + 3872y = 2: x = %ld, y = %ld\n", solution.x, solution.y);
	printf("Correct: x = 255, y = -179\n");
	return 0;
}