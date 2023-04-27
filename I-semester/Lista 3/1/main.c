#include <stdio.h>
#include "funs.h"
int main(void)
{
 char slowo[255];
 printf("Wpisz wyraz: \n");
 scanf("%s", slowo);
 if(palindrom(slowo))
 {
  printf("Wyraz jest palindromem.\n");
 }
 else
 {
  printf("Wyraz NIE jest palindromem.\n");
 }
 return 0;
}
