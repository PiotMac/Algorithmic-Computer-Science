#include "Figura.hpp"
#include "Kolo.hpp"
#include "Pieciokat.hpp"
#include "Szesciokat.hpp"
#include "Kwadrat.hpp"
#include "Prostokat.hpp"
#include "Romb.hpp"
#include <math.h>
#include <string>
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

bool czyKwadrat(double liczby[])
{
    //sprawdzenie czy kąt jest prosty i czy występują cztery boki tej samej dlugości
    if(liczby[4] == 90.0 && std::count(liczby, liczby + 4,liczby[0]) == 4)
    {
        return true;
    }
    return false;
}

bool czyProstokat(double liczby[])
{
    if(liczby[4] == 90.0 && std::count(liczby, liczby + 4,liczby[0]) == 2 && std::count(liczby, liczby + 4,liczby[2]) == 2)
    {
        return true;
    }
    return false;
}

bool czyRomb(double liczby[])
{
    if((liczby[4] < 180.0 && std::count(liczby, liczby + 4,liczby[0]) == 4))
    {
        return true;
    }
    return false;
}

int main (int argc, char* argv[])
{
    if (argv[1] == NULL)
    {
        cerr << "Brak argumentów!" << endl;
        return 0;
    }
    string parametry = argv[1];
    vector<Figura*> figury;
    int iterator = 2;
    
    for (size_t i = 0; i < parametry.size(); i++)
    {
        switch (parametry.at(i))
        {
            case 'o':
            {
                try
                {
                    if (stof(argv[iterator]) <= 0.0)
                    {
                        throw std::invalid_argument("Błąd: podana wartość jest niedodatnia lub nie jest typu double!");
                    }
                    Figura* kolo = new Kolo(stof(argv[iterator]));
                    //kolo -> promien = stof(argv[iterator]); //zamiana stringa na float
                    //promien = stof(argv[iterator]);
                    //figury[i] = kolo;
                    figury.push_back(kolo);
                }
                catch (std::invalid_argument& e)
                {
                    cout << "Niepoprawne dane." << endl;
                }
                iterator++;
                break;
            }
            case 'p':
            {
                try
                {
                    if (stof(argv[iterator]) <= 0.0)
                    {
                        throw std::invalid_argument("Błąd: podana wartość jest niedodatnia lub nie jest typu double!");
                    }
                    Figura* pieciokat = new Pieciokat(stof(argv[iterator]));
                    //pieciokat -> bok = stof(argv[iterator]); //zamiana stringa na float
                    //figury[i] = pieciokat;
                    figury.push_back(pieciokat);
                }
                catch (std::invalid_argument& e)
                {
                    cout << "Niepoprawne dane." << endl;
                }
                iterator++;
                break;
            }
            case 's':
            {
                try
                {
                    if (stof(argv[iterator]) <= 0.0)
                    {
                        throw std::invalid_argument("Błąd: podana wartość jest niedodatnia lub nie jest typu double!");
                    }
                    Figura* szesciokat = new Szesciokat(stof(argv[iterator]));
                    //szesciokat -> bok = stof(argv[iterator]); //zamiana stringa na float
                    //figury[i] = szesciokat;
                    figury.push_back(szesciokat);
                }
                catch (std::invalid_argument& e)
                {
                    cout << "Niepoprawne dane." << endl;
                }
                iterator++;
                break;
            }
            case 'c':
            {
                try
                {
                    double liczby[5] = {stof(argv[iterator]), stof(argv[iterator + 1]), stof(argv[iterator + 2]), stof(argv[iterator + 3]), stof(argv[iterator + 4])};
                    if (czyKwadrat(liczby))
                    {
                        Figura* kwadrat = new Kwadrat(stof(argv[iterator]));
                        //kwadrat -> bok1 = stof(argv[iterator]);
                        //figury[i] = kwadrat;
                        figury.push_back(kwadrat);
                    }
                    else if (czyProstokat(liczby))
                    {
                        Figura* prostokat = new Prostokat(stof(argv[iterator]), stof(argv[iterator + 2]));
                        //prostokat -> bok1 = stof(argv[iterator]);
                        //prostokat -> bok2 = stof(argv[iterator + 2]);
                        //figury[i] = prostokat;
                        figury.push_back(prostokat);
                    }
                    else if (czyRomb(liczby))
                    {
                        Figura* romb = new Romb(stof(argv[iterator]), stof(argv[iterator + 4]));
                        //romb -> bok1 = stof(argv[iterator]);
                        //romb -> kat = stof(argv[iterator + 4]);
                        //figury[i] = romb;
                        figury.push_back(romb);
                    }
                    else
                    {
		         cout << "Podany kąt jest zły lub dane zostały źle wprowadzone i nie pasują do rombu, prostokątu i kwadratu!" << endl;
                    }
                }
                catch (std::invalid_argument& e)
                {
                    cout << "Niepoprawne dane." << endl;
                }
                iterator += 5;
                break;
                
            }
            default:
            {
                cout << parametry.at(i) << " - błędna litera!" << endl;
            }
        }
    }
    
    if (iterator < argc)
    {
        cout << "Błąd: Podano za dużo argumentów" << endl;
    }
    for (size_t j = 0; j < parametry.size(); j++)
    {
        cout << "Nazwa: " << figury[j] -> wezNazwa() << endl;
        cout << "Pole: " << figury[j] -> pole() << endl;
        cout << "Obwód: " << figury[j] -> obwod() << endl;
        cout << endl;
        figury[j] -> ~Figura();
    }
}
		     
