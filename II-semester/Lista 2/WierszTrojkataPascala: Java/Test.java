public class Test
{
    public static void main (String[] args)
    {
        try
        {
            int wiersz = Integer.parseInt(args[0]);
            try
            {
                if (wiersz < 0 || wiersz > 61)
                {
                    throw new NieprawidlowyZakres();
                }
                
                WierszTrojkataPascala dana = new WierszTrojkataPascala(wiersz);
                for (int i = 1; i < args.length; i++)
                {
                    try
                    {
                        int n = Integer.parseInt(args[i]);
                        
                        try
                        {
                            if (n < 0 || n > wiersz)
                            {
                                throw new SpozaZakresu();
                            }
                            else
                            {
                                long x = dana.wspolczynnik(n);
                                System.out.println(args[i] + " - " + x);
                            }
                        }
                        catch (SpozaZakresu e)
                        {
                            System.out.println(args[i] + " - liczba spoza zakresu");
                        }
                    }
                    catch (NumberFormatException ex)
                    {
                        System.out.println(args[i] + " - nieprawidłowa dana");
                    }
                }
            }
            catch (NieprawidlowyZakres ee)
            {
                System.out.println(args[0] + " - nieprawidłowy zakres");
            }
        }
        catch (NumberFormatException ex)
        {
            System.out.println(args[0] + " - nieprawidłowy zakres");
        }
    }
}
