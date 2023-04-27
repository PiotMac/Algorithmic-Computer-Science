#include <stdio.h>
#include <assert.h>

int main()
{
int złote;
int grosze;
printf("Wprowadź liczbę złotych: ");
scanf("%d",&złote);
printf("Wprowadź liczbę groszy: ");
scanf("%d",&grosze);
assert(grosze<100);

int złotówki[8]={200, 100, 50, 20, 10, 5, 2, 1};
int groszówki[6]={50, 20, 10, 5, 2, 1};

printf("banknoty:\n");

for(int i=0;i<5;i++)
{
 if(złote>=złotówki[i])
 {
  int iloraz1=złote/złotówki[i];
  złote=złote-iloraz1*złotówki[i];
  printf("%d x %d zł\n",iloraz1,złotówki[i]);
 }
}

printf("monety:\n");

for(int i=5;i<8;i++)
{
 if(złote>=złotówki[i])
 {
  int iloraz1=złote/złotówki[i];
  złote=złote-iloraz1*złotówki[i];
  printf("%d x %d zł\n",iloraz1,złotówki[i]);
 }
}

for(int j=0;j<6;j++)
{
 if(grosze>=groszówki[j])
 {
  int iloraz2=grosze/groszówki[j];
  grosze=grosze-iloraz2*groszówki[j];
  printf("%d x %d gr\n",iloraz2,groszówki[j]);
 }
}
return 0;
}
