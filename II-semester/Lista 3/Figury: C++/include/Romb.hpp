#include "Czworokat.hpp"

class Romb : public Czworokat
{
    public:
        Romb(double, double);
        ~Romb();
        const char* wezNazwa();
        double pole();
        double obwod();
};
