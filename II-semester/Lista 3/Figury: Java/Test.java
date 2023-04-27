import java.util.*;
import java.lang.*;

public class Test
{
    public static boolean czyKwadrat (ArrayList <Double> argumenty)
    {
        ArrayList <Double> kwadrat = new ArrayList <Double> (argumenty);
        if(kwadrat.get(4) == 90.0 && kwadrat.size() == 5)
        {
            kwadrat.remove(90.0);
            if(new HashSet <Double> (kwadrat).size() == 1)
            {
                return true;
            }
        }
        return false;
    }

    public static boolean czyProstokąt (ArrayList <Double> argumenty)
    {
        ArrayList <Double> prostokąt = new ArrayList <Double> (argumenty);
        if ((prostokąt.get(4) == 90.0) && (prostokąt.size() == 5))
        {
            prostokąt.remove(90.0);
            Collections.sort(prostokąt);
            if(new HashSet <Double> (prostokąt).size() == 2 && (prostokąt.get(0)).equals(prostokąt.get(1)) && (prostokąt.get(2)).equals(prostokąt.get(3)))
            {
                return true;
            }
        }
        return false;
    }

    public static boolean czyRomb (ArrayList <Double> argumenty)
    {
        ArrayList <Double> romb = new ArrayList <Double> (argumenty);
        if(romb.get(4) > 180.0)
        {
            return false;
        }
        else
        {
            if(romb.size() == 5)
            {
                romb.remove(4);
                Collections.sort(romb);
                if((new HashSet <Double> (romb).size() == 1 || new HashSet <Double> (romb).size() == 2) && (romb.get(0)).equals(romb.get(3)))
                {
                    return true;
                }
            }
            return false;
        }
    }

    public static ArrayList <Double> Parametry (String[] liczby) throws IllegalArgumentException
    {
        ArrayList <Double> argumenty = new ArrayList <Double> ();
        for (int i = 0; i < liczby.length; i++)
        {
            try
            {
                if (Double.parseDouble(liczby[i]) <= 0.0)
                {
                    throw new IllegalArgumentException();
                }
                else
                {
                    argumenty.add(Double.parseDouble(liczby[i]));
                }
            }
            catch (IllegalArgumentException e) 
            {
                System.out.println(liczby[i] + " - argument musi być > 0 i typu double!");
            }
        }
        return argumenty;
    }

    public static void main(String[] args) 
	{
		String litery = args[0];
		ArrayList<Figura> listaFigur = new ArrayList<Figura>();
		int iterator = 1;
		
		for (int i = 0; i < litery.length(); i++)
		{
			switch (litery.charAt(i))
			{
				case 'o':
				{
					try
					{
						if (Double.parseDouble(args[iterator]) <= 0.0)
						{
							throw new IllegalArgumentException(args[iterator]);
						}
						
						Figura koło = new Koło(Double.parseDouble(args[iterator]));
						listaFigur.add(koło);
					}
					catch(IllegalArgumentException e)
					{
						System.out.println(args[iterator] + " - argument musi być > 0 i typu double!");
					}
					iterator++;
					break;
				}
				
				case 'c':
				{
					String[] liczby;
					liczby = Arrays.copyOfRange(args, iterator, iterator + 5);//kopiowanie 5 parametrów
					ArrayList <Double> argumenty = Parametry(liczby);
					
					if(czyKwadrat(argumenty))
					{
						Figura kwadrat = new Kwadrat(argumenty);
						listaFigur.add(kwadrat);
					}
					else if(czyProstokąt(argumenty))
					{ 
						Figura prostokąt = new Prostokąt(argumenty);
						listaFigur.add(prostokąt);
					}
					else if(czyRomb(argumenty))
					{   
						Figura romb = new Romb(argumenty);
						listaFigur.add(romb);
					}
					else
					{
					        System.out.println("Podany kąt jest zły lub dane zostały źle wprowadzone i nie pasują do rombu, prostokątu i kwadratu!");
					}
					
					iterator += 5;
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
						
						Figura pięciokąt = new Pięciokąt(Double.parseDouble(args[iterator]));
						listaFigur.add(pięciokąt);
						
					}
					catch(IllegalArgumentException e)
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
						
						Figura sześciokąt = new Sześciokąt(Double.parseDouble(args[iterator]));
						listaFigur.add(sześciokąt);
						
					}
					catch(IllegalArgumentException e)
					{
						System.out.println(args[iterator] + " - argument musi być > 0 i typu double!");
					}
					iterator++;
					break;
				}
				
				default:
				{
					System.out.println(litery.charAt(i) + " - błędna litera!");
				}
			}
		}
		for (int i = 0; i < listaFigur.size(); i++)
		{
			System.out.println("Nazwa: " + listaFigur.get(i).wezNazwa());
			System.out.println("Pole: " + listaFigur.get(i).pole());
			System.out.println("Obwód: " + listaFigur.get(i).obwod());
			System.out.println();
		}
	if (iterator < args.length)
        {
            System.out.println("Podano za dużo argumentów!");
        }
	}
}

