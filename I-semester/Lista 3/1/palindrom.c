#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "funs.h"
bool palindrom(char napis[])
{
 int i=0;
 int dl=strlen(napis);
 while(napis[i]==napis[dl-i-1] && i<=(dl/2))
 {
  i++;
 }
 if((i-1)==(dl/2))
 {
  return true;
 }
 else
 {
  return false;
 }
}
