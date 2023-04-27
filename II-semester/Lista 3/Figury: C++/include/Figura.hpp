#pragma once

class Figura
{
    public:
        virtual double pole() = 0;
        virtual double obwod() = 0;
        virtual const char* wezNazwa() = 0;
        Figura();
        virtual ~Figura();
};
