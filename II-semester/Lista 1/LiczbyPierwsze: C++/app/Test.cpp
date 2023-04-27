#include <iostream>
#include "header.hpp"

using namespace std;

int main (int argc, char *argv[])
{
    int zakres;
    try
    {
        zakres = stoi(argv[1]); //convert string to integer
        if (zakres >= 2)
        {
            static LiczbyPierwsze *dana = new LiczbyPierwsze(zakres);
            for (int i = 2; i < argc; i++)
            {
                try
                {
                    int n = stoi(argv[i]);
                    int iloscPierwszych = 0;
                    for (unsigned int x = 2; x < unsigned(zakres); x++)
                    {
                        if (dana->tablica[x])
                        {
                            iloscPierwszych++;
                        }
                    }
                    if (n >= iloscPierwszych || n < 0)
                    {
                        cout << n << " - liczba spoza zakresu." << endl;
                    }
                    else
                    {
                    	 int y = dana->liczba(n);
                        cout << n << " - " << y << endl;
                    }
                }
                catch (std::invalid_argument& ia)
                {
                    cout << argv[i] << " - nieprawidłowa dana." << endl;
                }
            }
            delete dana;
        }
        else
        {
            cout << zakres << " - nieprawidłowy zakres." << endl;
        }
    }
    catch (std::invalid_argument& ia)
    {
        cout << argv[1] << " - nieprawidłowy zakres." << endl;
    }
    return 0;
}
