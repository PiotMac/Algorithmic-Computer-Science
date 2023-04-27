#include "Kolo.hpp"
#include "Pieciokat.hpp"
#include "Szesciokat.hpp"
#include "Kwadrat.hpp"
#include "Prostokat.hpp"
#include "Romb.hpp"
#include <math.h>

Figura::Figura(){}
Figura::~Figura(){}
Czworokat::Czworokat(){}
Czworokat::~Czworokat(){}

Kolo::Kolo(double r)
{
    promien = r;
}

double Kolo::obwod()
{
    return 2.0 * promien * M_PI;
}

double Kolo::pole()
{
    return M_PI * pow(promien, 2);
}

const char* Kolo::wezNazwa()
{
    return "Koło";
}

Kolo::~Kolo(){}

Pieciokat::Pieciokat(double a)
{
    bok = a;
}

double Pieciokat::obwod()
{
    return 5.0 * bok;
}

double Pieciokat::pole()
{
    return 1.0/4.0 * pow(bok, 2) * sqrt(25.0 + 10.0 * sqrt(5.0));
}

const char* Pieciokat::wezNazwa()
{
    return "Pięciokąt";
}

Pieciokat::~Pieciokat(){}

Szesciokat::Szesciokat(double a)
{
    bok = a;
}

double Szesciokat::obwod()
{
    return 6.0 * bok;
}

double Szesciokat::pole()
{
    return 3.0/2.0 * pow(bok, 2) * sqrt(3.0);
}

const char* Szesciokat::wezNazwa()
{
    return "Sześciokąt";
}

Szesciokat::~Szesciokat(){}

Kwadrat::Kwadrat(double a)
{
    bok1 = a;
}

double Kwadrat::obwod()
{
    return 4.0 * bok1;
}

double Kwadrat::pole()
{
    return pow(bok1, 2);
}

const char* Kwadrat::wezNazwa()
{
    return "Kwadrat";
}

Kwadrat::~Kwadrat(){}

Prostokat::Prostokat(double a, double b)
{
    bok1 = a;
    bok2 = b;
}

double Prostokat::obwod()
{
    return 2.0 * bok1 + 2.0 * bok2;
}

double Prostokat::pole()
{
    return bok1 * bok2;
}

const char* Prostokat::wezNazwa()
{
    return "Prostokąt";
}

Prostokat::~Prostokat(){}

Romb::Romb(double a, double b)
{
    bok1 = a;
    kat = b;
}

double Romb::obwod()
{
    return 4.0 * bok1;
}

double Romb::pole()
{
    return pow(bok1, 2) * sin(kat * (M_PI/180.0));
}

const char* Romb::wezNazwa()
{
    return "Romb";
}

Romb::~Romb(){}
