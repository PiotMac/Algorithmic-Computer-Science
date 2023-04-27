#include <stdio.h>
float lnx(float);
float ex(float);

int main() {
	float input;
	float result;
	float ex1;
	float ex2;
	printf("Enter your number: \n");
	scanf("%f", &input);
	
	result = lnx(input);
	printf("ln(%f) = %f\n", input, result);
	
	result = ex(input);
	printf("e^(%f) = %f\n", input, result);
	
	ex1 = result;  //e^x
	ex2 = ex(0.0 - input); //e^(-x)
	result = (ex1-ex2)/2.0;
	printf("sinh(%f) = %f\n", input, result);
	
	result = 2.0/(ex1-ex2);
	printf("sinh-1(%f) = %f\n", input, result);
	
	
	return 0;
}

