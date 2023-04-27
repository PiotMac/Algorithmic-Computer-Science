#include <stdio.h>
int main()
{
 int n=0;
 float suma=0.0;
 while(suma<=10.0)
 {
  n++;
  suma=suma+(1.0/n);
 }
 printf("%d\n",n);
 return 0;
}
