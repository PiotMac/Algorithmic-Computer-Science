#include <stdio.h>

int main(void)
{
int n;
printf("Wpisz liczbę całkowitą od 1 do 20: \n");
scanf("%d",&n);

int wiersz=n;
int kolumna=2*n;

for(int i=1;i<=wiersz;i++)
{
 for(int j=1;j<=kolumna;j++)
 {
  printf("*");
 }
 printf("\n");
}
return 0;
}
