#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool match(char* wzorzec, char* lancuch)
{
 int dlugosc_lancucha = strlen(lancuch);
 int dlugosc_wzorca = strlen(wzorzec);
 int i=0;
   
 if(dlugosc_lancucha * dlugosc_wzorca == 0 && dlugosc_lancucha==dlugosc_wzorca)
 {
  return true;
 }
 else
 {
  if(wzorzec[0]=='*')
  {
   while(i<=dlugosc_lancucha)
   {
    if(match(wzorzec+1, lancuch+i))
    {
     return true;
    }
    i++;
   }
   return false;
  }
  else if(wzorzec[0]=='?' || wzorzec[0]==lancuch[0])
  {
   return match(wzorzec+1, lancuch+1);
  }
  else
  {
   return false;
  }
 }
 return false;
}
int main()
{
 char s1[200];
 char s2[200];
 printf("Wpisz wzorzec: \n");
 scanf("%s",s1);
 printf("Wpisz łańcuch: \n");
 scanf("%s",s2);
 if (match(s1,s2))
 {
  printf("Łańcuch jest zgodny ze wzorcem.\n");
 }
 else
 {
  printf("Łańcuch NIE jest zgodny ze wzorcem.\n");
 }
 return 0;
}
