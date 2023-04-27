#include "Figura.hpp"

class Kolo : public Figura
{
    public:
        double promien;
        Kolo(double);
        virtual ~Kolo();
        const char* wezNazwa();
        double pole();
        double obwod();
};
