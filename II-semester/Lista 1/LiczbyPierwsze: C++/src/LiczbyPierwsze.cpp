#include <iostream>
#include "header.hpp"

LiczbyPierwsze::LiczbyPierwsze(int n)
{
    for (int i = 0; i <= n + 1; i++)
    {
        tablica.push_back(true);
    }
    
    tablica[0] = false;
    tablica[1] = false;
    
    for (unsigned int i = 2; i * i <= unsigned(n + 1); i++)
    {
        if (tablica[i])
        {
            for (unsigned int j = i + i; j <= unsigned(n + 1); j = j + i)
            {
                tablica[j] = false;
            }
        }
    }
}


unsigned int LiczbyPierwsze::liczba(int m)
{
    {
    	int ktoraPierwsza = 0; //m-ta liczba pierwsza w tablicy
    	unsigned int mtaPierwsza = 0;
    	for (unsigned int i = 2; i < tablica.size(); i++)
        {
  	    if (tablica[i])
    	    {
    	        mtaPierwsza = i;
                if (ktoraPierwsza == m) //znaleźliśmy m-tą liczbę pierwszą
                {
                    break;
                }
                ktoraPierwsza++;
    	    }
    	}
    	return mtaPierwsza;
    }
}

