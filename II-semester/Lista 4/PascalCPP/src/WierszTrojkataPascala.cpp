#include <iostream>
#include "header.hpp"

WierszTrojkataPascala::WierszTrojkataPascala(int n)
{
    tablica = new unsigned long long[n+1];

    for (int k = 0; k <= n; k++)
    {
        if (k == 0 || k == n)
        {
            tablica[k] = 1;
        }
        else
        {
            unsigned long long p = 1;
            for (int i = 1; i <= k; i++)
            {
                p = (p * (n - i + 1)) / i;
                tablica[k] = p;
            }
        }
    }
}

unsigned long long WierszTrojkataPascala::wspolczynnik(int m)
{
    return tablica[m];
}

WierszTrojkataPascala::~WierszTrojkataPascala()
{
    delete tablica;
}
