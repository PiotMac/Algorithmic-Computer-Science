import java.lang.Math;

public class dzielniki 
{
    public static void main(String[] args)
    {
        for(int i = 0; i < args.length; i++)
        {
            try 
            {
                int n = Integer.parseInt(args[i]);
                System.out.println("Największym dzielnikiem " + n + " jest: " + div(n));
            }
            catch (NumberFormatException ex)
            {
                System.out.println(args[i] + " wyrażenie to nie jest liczbą calkowitą!");
            }
        }
    }

    public static int div(int n)
    {
        for(int i = 2; i <= Math.sqrt(n); i++)
        {
            if(n % i == 0)
            {
                return (n / i);
            }
        }
        return 1;
    }
}
