import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.layout.GridPane;
import java.util.Random;
import java.util.*;
//import javafx.beans.binding.Bindings;

/**
 * Główna klasa programu
 * @version 1.0
 * @author Piotr Maciejończyk
 * @param n - liczba kolumn w gridzie
 * @param m - liczba wierszy w gridzie
 * @param szybkosc - opóźnienie z jakim działa wątek
 * @param prawdopodobienstwo - liczba determinująca prawdopodobieństwo danego zdarzenia
 * @param losowy - generator liczb losowych
 * @param kwadraty - dwuwymiarowa tablica kwadratów na gridzie
 * @param sasiedzi - lista sąsiadów każdego wątku
 */
public class Symulacja extends Application
{
    //Deklaracja zmiennych globalnych
    private static int n,m;
    private static double szybkosc, prawdopodobienstwo;
    public static Random losowy = new Random();
    public MojWatek[][] kwadraty;
    public static List<MojWatek> sasiedzi;
    public static GridPane gridPane;

    /**
     * Metoda inicjująca działanie programu
     * @param stage - główny panel interfejsu
     */
    @Override
    public void start(Stage stage)
    {
        //Tworzenie dwuwymiarowej tablicy typu MojWatek
        kwadraty = new MojWatek[n][m];
        gridPane = new GridPane();

        //Dodawanie kwadratów do grida
        int iterator = 0;
        for (int kolumny = 0; kolumny < n; kolumny++)
        {
            for (int wiersze = 0; wiersze < m; wiersze++)
            {
                kwadraty[kolumny][wiersze] = new MojWatek(20, 20, szybkosc, prawdopodobienstwo, iterator);
                kwadraty[kolumny][wiersze].widthProperty().bind(gridPane.widthProperty().divide(n));
                kwadraty[kolumny][wiersze].heightProperty().bind(gridPane.heightProperty().divide(m));
                gridPane.add(kwadraty[kolumny][wiersze], kolumny, wiersze);
                iterator++;
            }
        }

        gridPane.setGridLinesVisible(true);

        //Tworzenie ArrayListy sąsiadów dla każdego wątku
        sasiedzi = new ArrayList<MojWatek>();
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < m; j++)
            {
                {
                    MojWatek lewy = kwadraty[(i - 1 + n) % n][j];
                    MojWatek prawy = kwadraty[(i + 1) % n][j];
                    MojWatek gorny = kwadraty[i][(j + 1) % m];
                    MojWatek dolny = kwadraty[i][(j - 1 + m) % m];
                    sasiedzi.add(lewy);
                    sasiedzi.add(prawy);
                    sasiedzi.add(gorny);
                    sasiedzi.add(dolny);
                }
            }
        }

        //Uruchamianie każdego wątku
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < m; j++)
            {
                MojWatek www = kwadraty[i][j];
                Thread w = new Thread(www);
                w.start();
            }
        }

        //Ustawianie sceny i stage'a
        Scene symulacja = new Scene(gridPane, 20 * n, 20 * m, Color.BEIGE);
        stage.setTitle("Symulacja");
        stage.setScene(symulacja);
        stage.show();
    }

    /**
     * Metoda main programu
     * @param args - użytkownik podaje rozmiary okna grida, opóźnienie wątków oraz prawdopodobieństwo
     */
    public static void main(String[] args) 
    {
        try
        {
            n = Integer.parseInt(args[0]);
            m = Integer.parseInt(args[1]);
            szybkosc = Double.parseDouble(args[2]);
            prawdopodobienstwo = Double.parseDouble(args[3]);
            if (n <= 0 || m <= 0 || szybkosc < 0.0 || prawdopodobienstwo > 1.0 || prawdopodobienstwo < 0.0)
            {
                throw new IllegalArgumentException();
            }
        }
        catch(IllegalArgumentException e)
        {
            System.out.println("Źle wprowadzone dane!");
            System.exit(0);
        }
        launch(args);
    }
}
