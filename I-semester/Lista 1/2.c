#include <stdio.h>
#include <math.h>

int main(void)
{
float a=0,b=0,c=0;
printf("Podaj współczynnik 'a'\n");
scanf("%f",&a);
printf("Podaj współczynnik 'b'\n");
scanf("%f",&b);
printf("Podaj współczynnik 'c'\n");
scanf("%f",&c);

float delta=(b*b)-(4*a*c);
if(a==0)
{
 printf("To nie jest równanie kwadratowe.\n");
}
else
{
  if(delta>0)
  {
    printf("Pierwsze rozwiązanie to: %f\n",(-b-sqrt(delta))/(2*a));
    printf("Drugie rozwiązanie to: %f\n",(-b+sqrt(delta))/(2*a));
  }
  else
  {
    if(delta==0)
    {
     printf("Jedyne rozwiązanie to: %f\n",-b/(2*a));
    }
    else
    {
      if(delta<0)
      {
       printf("Brak rozwiązań.");
      }
    }
  }
}
return 0;
}
