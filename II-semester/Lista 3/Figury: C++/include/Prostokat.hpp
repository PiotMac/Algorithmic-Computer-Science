#include "Czworokat.hpp"

class Prostokat : public Czworokat
{
    public:
        Prostokat(double, double);
        ~Prostokat();
        const char* wezNazwa();
        double pole();
        double obwod();
};
