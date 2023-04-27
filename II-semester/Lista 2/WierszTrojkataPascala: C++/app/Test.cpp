#include <iostream>
#include "header.hpp"

int main (int argc, char *argv[])
{
    int wiersz;
    try
    {
    	if(argv[1] == NULL)
    	{
    	    return 0;
    	}
        wiersz = stoi(argv[1]);
        try
        {
            if (wiersz < 0 || wiersz > 62)
            {
                throw (string) " - nieprawidłowy zakres.";
            }
                WierszTrojkataPascala *dana = new WierszTrojkataPascala(wiersz);
                for (int i = 2; i < argc; i++)
                {
                    try
                    {
                        int n = stoi(argv[i]);
                        try
                        {
                            if (n > wiersz || n < 0)
                            {
                                throw (string) " - liczba spoza zakresu.";
                            }
                            else
                            {
                    	         unsigned long long x = dana->wspolczynnik(n);
                                cout << n << " - " << x << endl;
                            }
                        }
                        catch (string wiadomosc)
                        {
                            cout << argv[i] << wiadomosc << endl;
                        }
                    }
                    catch (std::invalid_argument& ia)
                    {
                        cout << argv[i] << " - nieprawidłowa dana." << endl;
                    }
                }
                delete dana;
        }
        catch (string wiadomosc)
        {
            cout << wiersz << wiadomosc << endl;
        }
    }
    catch (std::invalid_argument& ia)
    {
        cout << argv[1] << " - nieprawidłowy zakres." << endl;
    }
    return 0;
}
