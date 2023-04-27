import java.util.Arrays;

public class LiczbyPierwsze 
{
    public static boolean tablica[];

    public LiczbyPierwsze (int n)
    {
     	tablica = new boolean[n + 1];
     	Arrays.fill(tablica, true); //wypełnienie tablicy wartościami true

        tablica[0] = false;
        tablica[1] = false;

        for (int i = 2; i * i <= n; i++)
        {
            if (tablica[i])
            {
                for (int j = i + i; j <= n; j = j + i)
                {
                    tablica[j] = false;
                }
            }
        }
    }

    public static int liczba (int m)
    {
        int ktoraPierwsza = 0; //m-ta liczba pierwsza w tablicy
        int mtaPierwsza = 0;
        for (int i = 2; i < tablica.length; i++)
        {
            if (tablica[i])
            {
                mtaPierwsza = i;
                if (ktoraPierwsza == m) //znaleźliśmy m-tą liczbę pierwszą
                {
                    break;
                }
                ktoraPierwsza++;
            }
        }
        return mtaPierwsza;
    }
}

