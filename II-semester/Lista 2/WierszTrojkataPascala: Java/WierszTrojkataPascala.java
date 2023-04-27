public class WierszTrojkataPascala
{
    private long tablica[];

    public WierszTrojkataPascala(int n)
    {
        tablica = new long[n+1];

        for (int k = 0; k <= n; k++)
        {
            if (k == 0 || k == n)
            {
                tablica[k] = 1;
            }
            else
            {
                long p = 1;
                for (int i = 1; i <= k; i++)
                {
                    p = (p * (n - i + 1)) / i;
                    tablica[k] = p;
                }
            }
        }
    }
    
    public long wspolczynnik(int m)
    {
        return tablica[m];
    }
}

