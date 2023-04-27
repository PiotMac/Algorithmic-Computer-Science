public class Test
{
    public static void main (String[] args)
    {
        try
        {
            int zakres = Integer.parseInt(args[0]);
            if (zakres > 2)
            {
                LiczbyPierwsze dana = new LiczbyPierwsze(zakres);
                for (int i = 1; i < args.length; i++)
                {
                    try
                    {
                        int n = Integer.parseInt(args[i]);
                        int iloscPierwszych = 0;
                        for (int x = 2; x < zakres; x++)
                        {
                            if (dana.tablica[x])
                            {
                                iloscPierwszych++;
                            }
                        }
                        if (n >= iloscPierwszych || n < 0)
                        {
                            System.out.println(args[i] + " - liczba spoza zakresu");
                        }
                        else
                        {
                            int y = LiczbyPierwsze.liczba(n);
                            System.out.println(args[i] + " - " + y);
                        }
                    }
                    catch (NumberFormatException ex)
                    {
                        System.out.println(args[i] + " - nieprawidłowa dana");
                    }
                }
            }
            else
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

