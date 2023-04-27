#include <stdio.h>
#include <assert.h>
int main()
{
 int n;
 float suma=0;
 float k;
 printf("Podaj liczbę całkowitą: ");
 scanf("%d",&n);
 assert(n>0);
 for(int i=1;i<=n;i++)
 {
  printf("Wprowadź liczbę: \n");
  scanf("%f",&k);
  suma=suma+k;
 }
 float sr_arytm=suma/n;
 printf("Średnia arytmetyczna wprowadzonych liczb to: %f",sr_arytm);
 return 0;
}
