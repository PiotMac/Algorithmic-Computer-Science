import static java.lang.Math.*;
import java.util.*;

interface Interfejs1
{
    public double ObliczPole(double a);
    public double ObliczObwod(double a);
}
    
interface Interfejs2
{
    public double ObliczPole(double a, double b);
    public double ObliczObwod(double a, double b);
}

class Figura
{   
    public enum JedenParametr implements Interfejs1
    {
        KWADRAT
        {
            public double ObliczPole(double a)
            {
                return a * a;
            }
            public double ObliczObwod(double b)
            {
                return 4.0 * b;
            }
        },
        KOLO
        {
            public double ObliczPole(double a)
            {
                return a * a * PI;
            }
            public double ObliczObwod(double b)
            {
                return 2.0 * PI * b;
            }
        },
        PIECIOKAT
        {
            public double ObliczPole(double a)
            {
                return 1.0/4.0 * pow(a, 2) * sqrt(25.0 + 10.0 * sqrt(5.0));
            }
            public double ObliczObwod(double b)
            {
                return 5.0 * b;
            }
        },
        SZESCIOKAT
        {
            public double ObliczPole(double a)
            {
                return 3.0/2.0 * pow(a, 2) * sqrt(3.0);
            }
            public double ObliczObwod(double b)
            {
                return 6.0 * b;
            }
        };
    }
    
    public enum DwaParametry implements Interfejs2
    {
        PROSTOKAT
        {
            public double ObliczPole(double a, double b)
            {
                return a * b;
            }
            public double ObliczObwod(double c, double d)
            {
                return 2.0 * c + 2.0 * d;
            }
        },
        ROMB
        {
            public double ObliczPole(double a, double kat)
            {
                return a * a * sin(toRadians(kat));
            }
            public double ObliczObwod(double c, double d)
            {
                return 4 * c;
            }
        };
    }
}


