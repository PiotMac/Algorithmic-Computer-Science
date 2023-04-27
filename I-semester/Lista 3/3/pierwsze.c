#include "funs.h"
#include <stdbool.h>

bool pierwsze(long int i, long int n)
{
 for(int j=2; j<n; j++)
 {
  if(i%j==0 && n%j==0)
  {
   return false;
  }
 }
 return true;
}
