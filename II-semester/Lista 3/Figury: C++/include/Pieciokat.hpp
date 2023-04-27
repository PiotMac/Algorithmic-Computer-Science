#include "Figura.hpp"

class Pieciokat : public Figura
{
    public:
        double bok;
        Pieciokat(double);
        ~Pieciokat();
        const char* wezNazwa();
        double pole();
        double obwod();
};
