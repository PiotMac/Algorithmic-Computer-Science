import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.application.Platform;
//import javafx.scene.layout.GridPane;

/**
 * Klasa odpowiedzialna za działanie każdego wątku
 */
public class MojWatek extends Rectangle implements Runnable
{
    double k;
    double p;
    int iterator;
    //GridPane grid = Symulacja.gridPane;

    //Losowanie początkowych kolorów kwadratu
    int r = Symulacja.losowy.nextInt(256);
    int g = Symulacja.losowy.nextInt(256);
    int b = Symulacja.losowy.nextInt(256);

    /**
     * \brief Podstawowy konstruktor klasy MojWatek
     * @param szerokosc - szerokość kwadratu
     * @param wysokosc - wysokość kwadratu
     * @param szybkosc - opóźnienie działania wątku
     * @param prawdopodobienstwo - zmienna określająca prawdopodobieństwo wystąpienia danej akcji
     * @param iterator - określa, którym z kolei stworzonym kwadratem jest dany wątek
     */
    MojWatek(int szerokosc, int wysokosc, double szybkosc, double prawdopodobienstwo, int iterator)
    {
        //Tworzenie kwadratu
        super(szerokosc, wysokosc);
        super.setFill(Color.rgb(r, g, b));

        //Przypisywanie wartości do zmiennych globalnych
        this.iterator = iterator;
        k = szybkosc;
        p = prawdopodobienstwo;
    }

    /**
     * Metoda odpowiedzialna za rozpoczęcie wątku
     */
    @Override
    public void run()
    {
        while(true)
        {
            //synchronized(this)
            //{
            //Określanie opóźnienia
            double delay = 0.5 * k + (1.5 * k - 0.5 * k) * Symulacja.losowy.nextDouble();

            //Losowanie zmiennych
            double zmienKolor = Symulacja.losowy.nextDouble();
            int r1 = Symulacja.losowy.nextInt(256);
            int g1 = Symulacja.losowy.nextInt(256);
            int b1 = Symulacja.losowy.nextInt(256);

            //Zmiana koloru kwadratu na losowy
            if (zmienKolor <= p)
            {
                Platform.runLater(() -> super.setFill(Color.rgb(r1, g1, b1)));
            }
            //Zastąpienie koloru kwadratu średnią kolorów jego sąsiadów
            else
            {
                int red = 0;
                int green = 0;
                int blue = 0;
                //Pętla odpowiedzialna za pobranie kolorów każdego z sąsiadów
                for (int i = 0; i < 4; i++)
                {
                    Color kolor = (Color) Symulacja.sasiedzi.get(iterator * 4 + i).getFill();
                    red += ((int) Math.round(kolor.getRed() * 255));
                    green += ((int) Math.round(kolor.getGreen() * 255));
                    blue += ((int) Math.round(kolor.getBlue() * 255));
                }
                int czerwony = Math.round(red / 4);
                int zielony = Math.round(green / 4);
                int niebieski = Math.round(blue / 4);

                Platform.runLater(() -> super.setFill(Color.rgb(czerwony, zielony, niebieski)));
            }
            try { Thread.sleep((long) delay); } catch (InterruptedException e) {}
            //}
        }
    }
}
