import static java.lang.Math.*;
import java.util.*;

interface Pole
{
    public double pole();
}

interface Obwod
{
    public double obwod();
}

interface WezNazwa
{
    public String wezNazwa();
}

abstract class Figura implements Pole, Obwod, WezNazwa
{
    
}

abstract class Czworokąt extends Figura
{

}

class Romb extends Czworokąt
{
    private double bok;
    private double kąt;
    private String nazwa = "Romb";
    
    Romb(ArrayList <Double> argumenty)
    {
        bok = argumenty.get(0);
        kąt = argumenty.get(4);
    }
    public double pole()
    {
        return pow(bok, 2) * sin(toRadians(kąt));
    }
    public double obwod()
    {
        return 4.0 * bok;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}

class Prostokąt extends Czworokąt
{
    private double bok1;
    private double bok2;
    private String nazwa = "Prostokąt";
    
    Prostokąt(ArrayList <Double> argumenty)
    {
        argumenty.remove(90.0);
        bok1 = argumenty.get(0);
        bok2 = argumenty.get(2);
    }
    
    public double pole()
    {
        return bok1 * bok2;
    }
    public double obwod()
    {
        return 2.0 * bok1 + 2.0 * bok2;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}

class Kwadrat extends Czworokąt
{
    private double bok;
    private String nazwa = "Kwadrat";
    
    Kwadrat(ArrayList <Double> argumenty)
    {
        argumenty.remove(90.0);
        bok = argumenty.get(0);
    }
    
    public double pole()
    {
        return bok * bok;
    }
    public double obwod()
    {
        return 4.0 * bok;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}

class Koło extends Figura
{
    private double promień;
    private String nazwa = "Koło";
    
    Koło(double r)
    {
        promień = r;
    }
    public double pole()
    {
        return PI * pow(promień, 2);
    }
    public double obwod()
    {
        return 2.0 * PI * promień;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}

class Pięciokąt extends Figura
{
    private double bok;
    private String nazwa = "Pięciokąt";
    
    Pięciokąt(double a)
    {
        bok = a;
    }
    public double pole()
    {
        return 1.0/4.0 * pow(bok, 2) * sqrt(25.0 + 10.0 * sqrt(5.0));
    }
    public double obwod()
    {
        return 5.0 * bok;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}

class Sześciokąt extends Figura
{
    private double bok;
    private String nazwa = "Sześciokąt";
    
    Sześciokąt (double a)
    {
        bok = a;
    }
    public double pole()
    {
        return 3.0/2.0 * pow(bok, 2) * sqrt(3.0);
    }
    public double obwod()
    {
        return 6.0 * bok;
    }
    public String wezNazwa()
    {
        return nazwa;
    }
}
