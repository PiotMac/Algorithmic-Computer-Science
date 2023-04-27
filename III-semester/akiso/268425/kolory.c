#include <stdio.h>
int main() 
{
    for (int i = 0; i <= 255; i++)
    {
    	printf("\033[38;5;%dmHello, World!\n", i);
    }
    return 0;
}
