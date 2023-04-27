#include <stdio.h>
#include <math.h>
int main()
{
 int n=1000;
 int i=1;
 double wynik=1.0;
 while(i<=n)
 {
  wynik=wynik*pow(i, 0.001);
  i++;
 }
 printf("Wynik tego wyrażenia to: %f\n",wynik);
 return 0;
}
