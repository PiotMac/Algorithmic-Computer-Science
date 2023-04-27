#include "funs.h"
#include <stdio.h>
#include <assert.h>
int main()
{
 int n;
 printf("Wpisz liczbę naturalną: ");
 scanf("%d", &n);
 assert(n>=1);
 printf("Liczba %d ma tyle liczb, które są z nią względnie pierwsze: %d",n ,phi(n));
 return 0;
}
