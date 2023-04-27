#include "funs.h"
#include <stdio.h>

double rozwiazanie(double a, double b, double eps)
{
 if(f(a)==0.0)
 {
  return a;
 }
 if(f(b)==0.0)
 {
  return b;
 }
 double srodek;
 while(b-a>eps)
 {
  srodek=(a+b)/2;
  if(f(srodek)==0.0)
  {
   return srodek;
  }
  if(f(a)*f(srodek)<0)
  {
   b=srodek;
  }
  else
  {
   a=srodek;
  }
 }
 return (a+b)/2;
}
