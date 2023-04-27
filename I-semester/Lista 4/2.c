#include <stdio.h>
#include <assert.h>

int main(void)
{
 int kombinacje[6][6][6][6];
 for(int x=0;x<6;x++)
 {
  for(int y=0;y<6;y++)
  {
   for(int z=0;z<6;z++)
   {
    for(int t=0;t<6;t++)
    {
     kombinacje[x][y][z][t]=1;
    }
   }
  }
 }
 int a=0,b=0,c=0,d=0;
 int red,white;
 int liczba_kombinacji=1296;
 int red_pom=0;
 int white_pom=0;
 
 for(int k=0;k<8;k++)
 {
  while(liczba_kombinacji>0 && kombinacje[a][b][c][d]==0)
  {
   if(d<5)
   {
    d=d+1;
   }
   else if(d==5)
   {
    d=0;
    if(c<5)
    {
     c=c+1;
    }
    else if(c==5)
    {
     c=0;
     if(b<5)
     {
      b=b+1;
     }
     else if(b==5)
     {
      b=0;
      if(a<5)
      {
       a=a+1;
      }
      else if(a==5)
      {
       a=0;
       b=0;
       c=0;
       d=0;
      }
     }
    }
   }
  }

  if(liczba_kombinacji<=0)
  {
   printf("You are a cheater...\n");
   break;
  }
  printf("[%d] [%d] [%d] [%d]?\n", a+1, b+1, c+1, d+1);
  printf("red: \n");
  scanf("%d",&red);
  printf("white: \n");
  scanf("%d",&white);
  assert(red>=0 && white>=0 && red+white<=4);
  if(red==4)
  {
   printf("Hooray! I'm the winner!\n");
   k=8;
  }
  else if(liczba_kombinacji>0)
  {
   kombinacje[a][b][c][d]=0; 
   liczba_kombinacji=liczba_kombinacji-1;
   for(int x=0;x<6;x++)
   {
    for(int y=0 ;y<6;y++)
    {
     for(int z=0;z<6;z++)
     {
      for(int t=0;t<6;t++)
      {
       red_pom=0;
       white_pom=0;
       if(a==x)
       {
        red_pom=red_pom+1;
       }
       if(b==y)
       {
        red_pom=red_pom+1;
       }
       if(c==z)
       {
        red_pom=red_pom+1;
       }
       if(d==t)
       {
        red_pom=red_pom+1;
       }
       if(red_pom!=red && kombinacje[x][y][z][t]==1)
       {
       kombinacje[x][y][z][t]=0;
       liczba_kombinacji=liczba_kombinacji-1;
       }
       else if(red_pom==red)
       {
        if(x!=a && (x==b || x==c || x==d))
        {
         white_pom=white_pom+1;
        }
        if(y!=b && (y==a || y==c || y==d))
        {
         white_pom=white_pom+1;
        }
        if(z!=c && (z==a || z==b || z==d))
        {
         white_pom=white_pom+1;
        }
        if(t!=d && (t==a || t==b || t==c))
        {
         white_pom=white_pom+1;
        }
       }
       if(white_pom!=white && kombinacje[x][y][z][t]==1)
       {
        kombinacje[x][y][z][t]=0;
        liczba_kombinacji=liczba_kombinacji-1;
       }
      }
     }
    } 
   }
  }	 
 }
 if(liczba_kombinacji>0 && red!=4)
 {
  printf("Congratulations! You've defeated me...:( \n");
 }
 return 0;
}


