//Zadanie 4 z listy 1, Piotr Maciejończyk
#include <stdio.h>

int main(void)
{
int n;
printf("Podaj liczbę całkowitą: \n");
scanf("%d",&n);
int x=n;
for(int i=0;i<n;i++)
{
 x--;
 for(int j=0;j<x;j++)
  {
   printf(" ");
  }
 for(int k=0;k<(2*i)+1;k++)
  {
   printf("*");
  }
 printf("\n");
}
return 0;
}
