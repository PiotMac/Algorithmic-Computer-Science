#include "Figura.hpp"

class Szesciokat : public Figura
{
    public:
        double bok;
        Szesciokat(double);
        ~Szesciokat();
        const char* wezNazwa();
        double pole();
        double obwod();
};
