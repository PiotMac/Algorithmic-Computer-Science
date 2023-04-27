import java.util.*;
import static java.lang.Math.*;

public class FiguryEnumTest
{
    public static void main(String args[])
    {
        String litery = args[0];
        int iterator = 1;
        try
        {
            for (int i = 0; i < litery.length(); i++)
            {
                switch(litery.charAt(i))
                {
                    case 'o':
                    {
                        try
                        {
                            if (Double.parseDouble(args[iterator]) <= 0.0)
			     {
			         throw new IllegalArgumentException(args[iterator]);
			     }
			     double promien = Double.parseDouble(args[iterator]);
			     System.out.println("Koło o promieniu " + promien + " ma pole: " + Figura.JedenParametr.KOLO.ObliczPole(promien) + " oraz obwód: " + Figura.JedenParametr.KOLO.ObliczObwod(promien));
			     System.out.println();
                        }
                        catch (IllegalArgumentException e)
                        {
                            System.out.println(args[iterator] + " - argument musi być > 0 i typu double!");
                        }
                        iterator++;
                        break;
                    }
                    case 's':
                    {
                        try
                        {
                            if (Double.parseDouble(args[iterator]) <= 0.0)
			     {
			         throw new IllegalArgumentException(args[iterator]);
			     }
			     double bok = Double.parseDouble(args[iterator]);
			     System.out.println("Sześciokąt o boku " + bok + " ma pole: " + Figura.JedenParametr.SZESCIOKAT.ObliczPole(bok) + " oraz obwód: " + Figura.JedenParametr.SZESCIOKAT.ObliczObwod(bok));
			     System.out.println();
                        }
                        catch (IllegalArgumentException e)
                        {
                            System.out.println(args[iterator] + " - argument musi być > 0 i typu double!");
                        }
                        iterator++;
                        break;
                    }
                    case 'p':
                    {
                        try
                        {
                            if (Double.parseDouble(args[iterator]) <= 0.0)
			     {
			         throw new IllegalArgumentException(args[iterator]);
			     }
			     double bok = Double.parseDouble(args[iterator]);
			     System.out.println("Pięciokąt o boku " + bok + " ma pole: " + Figura.JedenParametr.PIECIOKAT.ObliczPole(bok) + " oraz obwód: " + Figura.JedenParametr.PIECIOKAT.ObliczObwod(bok));
			     System.out.println();
                        }
                        catch (IllegalArgumentException e)
                        {
                            System.out.println(args[iterator] + " - argument musi być > 0 i typu double!");
                        }
                        iterator++;
                        break;
                    }
                    case 'c':
                    {
                        if (Double.parseDouble(args[iterator + 4]) == 90.0)
                        {
                            if(Double.parseDouble(args[iterator]) == Double.parseDouble(args[iterator + 1]) && Double.parseDouble(args[iterator + 1]) == Double.parseDouble(args[iterator + 2]) && Double.parseDouble(args[iterator + 2]) == Double.parseDouble(args[iterator + 3]))
                            {
                                double bok = Double.parseDouble(args[iterator]);
                                System.out.println("Kwadrat o boku " + bok + " ma pole: " + Figura.JedenParametr.KWADRAT.ObliczPole(bok) + " oraz obwód: " + Figura.JedenParametr.KWADRAT.ObliczObwod(bok));
                                System.out.println();
                            }
                            else
                            {
                                double[] testbokow = {Double.parseDouble(args[iterator]), Double.parseDouble(args[iterator + 1]), Double.parseDouble(args[iterator + 2]), Double.parseDouble(args[iterator + 3])};
                                Arrays.sort(testbokow);
                                if (testbokow[0] == testbokow[1] && testbokow[2] == testbokow[3])
                                {
                                    double bok1 = testbokow[0];
                                    double bok2 = testbokow[2];
                                    System.out.println("Prostokąt o bokach " + bok1 + " oraz " + bok2 + " ma pole: " + Figura.DwaParametry.PROSTOKAT.ObliczPole(bok1, bok2) + " oraz obwód: " + Figura.DwaParametry.PROSTOKAT.ObliczObwod(bok1, bok2));
                                    System.out.println();
                                }
                                else
                                {
                                    System.out.println("Nieprawidłowe boki!");
                                    System.exit(0);
                                }
                            }
                        }
                        else if (Double.parseDouble(args[iterator + 4]) < 180.0 && Double.parseDouble(args[iterator + 4]) > 0.0 && Double.parseDouble(args[iterator]) == Double.parseDouble(args[iterator + 1]) && Double.parseDouble(args[iterator + 1]) == Double.parseDouble(args[iterator + 2]) && Double.parseDouble(args[iterator + 2]) == Double.parseDouble(args[iterator + 3]))
                        {
                            double bok = Double.parseDouble(args[iterator]);
                            double kat = Double.parseDouble(args[iterator + 4]);
                            System.out.println("Romb o boku " + bok + " oraz kącie " + kat + " stopni ma pole: " + Figura.DwaParametry.ROMB.ObliczPole(bok, kat) + " oraz obwód: " + Figura.DwaParametry.ROMB.ObliczObwod(bok, kat));
                            System.out.println();
                        }
                        else
                        {
                           System.out.println("Nieprawidłowy kąt lub boki niepasujące do rombów i prostokątów!");
                           System.exit(0);
                        }
                        iterator += 5;
                        break;
                    }
                    default:
                    {
                        System.out.println("Błędna litera!");
                    }
                }
            }
        }
        catch(ArrayIndexOutOfBoundsException ex)
        {
            System.out.println("Za mało danych!");
            System.exit(0);
        }
        catch(NumberFormatException ex)
        {
            System.out.println("Nieprawidłowe dane!");
            System.exit(0);
        }
        if (iterator < args.length)
        {
            System.out.println("Podano za dużo argumentów!");
        }
    }
}
