#include <stdio.h>
int main()
{
 int n;
 int suma=0;
 printf("Wszystkie liczby doskonałe mniejsze do 1000: \n");
 for(n=2;n<1000;n++)
 {
  for(int x=1;x<n;x++)
  {
   if(n%x==0)
   {
    suma=suma+x;
   }
  }
  if(suma==n)
  {
   printf("%d\n",n);
  }
  suma=0;
 }
 
 printf("Wszystkie liczby zaprzyjaźnione mniejsze do 1000 (wyświetlane parami): \n");
 int a,b;
 for(n=1;n<1000;n++)
 {
  int suma1=0;
  for(a=1;a<n;a++)
  {
   if(n%a==0)
   {
    suma1=suma1+a;
   }
  }
  int suma2=0;
  for(b=1;b<suma1;b++)
  {
   if(suma1%b==0)
   {
    suma2=suma2+b;
   }
  }
  if(a==suma2 && a!=b)
  {
   printf("%d\n", a);
  }
 }

return 0;
}
