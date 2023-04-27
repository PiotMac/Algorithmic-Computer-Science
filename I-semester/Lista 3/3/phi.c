#include "funs.h"
#include <stdbool.h>

int phi(long int n)
{
 int wynik=0;
 if(n==1)
 {
  wynik=1;
 }
 else
 {
  for(int i=1;i<n;i++)
  {
   if(pierwsze(i,n)==true)
   {
    wynik++;
   }
  }
 }
 return wynik;
}
