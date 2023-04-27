#include <stdio.h>

int NWD(int a, int b)
{
 int x;
 while(b!=0)
 {
  x=b;
  b=a%b;
  a=x;
 }
 return a;
}

int main()
{
 for(int n=1; n<=1000;n++)
 {
  float y=0.0;
  float z=0.0;
  for(int i=1;i<=n;i++)
  {
   for(int j=1;j<=n;j++)
   {
    if(NWD(i,j)==1)
    {
     y++;
    }
   }
  }
  z=y/(n*n);
  printf("%d; %f\n",n,z);
 }
return 0;
}
