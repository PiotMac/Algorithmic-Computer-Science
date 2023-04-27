#include "Czworokat.hpp"

class Kwadrat : public Czworokat
{
    public:
        Kwadrat(double);
        ~Kwadrat();
        const char* wezNazwa();
        double pole();
        double obwod();
};
