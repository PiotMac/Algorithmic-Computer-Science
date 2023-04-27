import javafx.scene.shape.Polygon;
import javafx.geometry.Bounds;
import javafx.event.EventHandler;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.Cursor;
import javafx.scene.input.ScrollEvent;
import javafx.scene.paint.Color;
import javafx.collections.ObservableList;

/**
 * Klasa dziedzicząca po klasie Polygon
 */
public class Trojkat extends Polygon
{
    double srodekX;
    double srodekY;
    /**
     * Podstawowy konstruktor
     * @param x1 współrzędna x pierwszego punktu
     * @param y1 współrzędna y pierwszego punktu
     * @param x2 współrzędna x drugiego punktu
     * @param y2 współrzędna y drugiego punktu
     * @param x3 współrzędna x trzeciego punktu
     * @param y3 współrzędna y trzeciego punktu
     */
    public Trojkat(double x1, double y1, double x2, double y2, double x3, double y3) 
    { 
        //wyznaczanie współrzędnych środka trójkąta
        srodekX = (x1 + x2 + x3) / 3.0;
        srodekY = (y1 + y2 + y3) / 3.0;

        this.getPoints().addAll(x1, y1, x2, y2, x3, y3);
        setFill(Color.YELLOW);
        setOnMouseClicked(new TrojkatEventHandler());
        setOnMouseDragged(new TrojkatEventHandler());
        setOnMouseEntered(new TrojkatEventHandler());
        setOnMouseExited(new TrojkatEventHandler());
        setOnScroll(new TrojkatScrollEventHandler());
    }

    /**
     * Metoda sprawdzająca czy użytkownik najechał kursorem na trójkąt
     * @param x współrzędna x kursora na panelu
     * @param y współrzędna y kursora na panelu
     * @return tak lub nie w zależności od tego czy kursor najechał na trójkąt
     */
    private boolean czyTrafiony(double x, double y)
    {
        return getBoundsInLocal().contains(x,y);
    }

    /**
     * Klasa implementująca przesuwanie trójkąta
     */
    class TrojkatEventHandler implements EventHandler<MouseEvent>
    {
        Trojkat trojkat;
        private double x;
        private double y;
        /** Dynamiczna lista punktów trójkąta */
        ObservableList<Double> punkty = getPoints();
  
        /**
         * Metoda odpowiedzialna za przesuwanie trójkąta
         * @param event zdarzenie używania myszki
         */
        private void przesun(MouseEvent event) 
        {   
            double dx = event.getX() - x;
            double dy = event.getY() - y;
  
            //Jeśli nacisnęliśmy na trójkąt
            if (trojkat.czyTrafiony(x, y)) 
            {
                for (int i = 0; i < 6; i++)
                {
                    if (i % 2 == 0)
                    {
                        punkty.set(i, punkty.get(i) + dx);
                    }
                    else
                    {
                        punkty.set(i, punkty.get(i) + dy);
                    }
                }
                srodekX = (punkty.get(0) + punkty.get(2) + punkty.get(4)) / 3.0;
                srodekY = (punkty.get(1) + punkty.get(3) + punkty.get(5)) / 3.0;
            }
            //Aktualizacja pozycji kursora
            x += dx;
            y += dy;            
        }

        /**
         * Nadpisana metoda handle wywołująca metodę przesuwania trójkąta
         * @param event zdarzenie używania myszki
         * @see TrojkatEventHandler#przesun(MouseEvent)
         */
        @Override
        public void handle(MouseEvent event) 
        {
            trojkat = (Trojkat) event.getSource();
            //Najeżdżając na trójkąt tworzy się obramowanie
            if (event.getEventType() == MouseEvent.MOUSE_ENTERED)
            {
                trojkat.setCursor(Cursor.HAND);
                trojkat.setStroke(Color.GREEN);
                trojkat.setStrokeWidth(8);
            }
            //Zdejmując kursor z trójkąta obramowanie znika
            if (event.getEventType() == MouseEvent.MOUSE_EXITED)
            {
                trojkat.setCursor(Cursor.DEFAULT);
                trojkat.setStrokeWidth(0);
            }
            if (event.getEventType() == MouseEvent.MOUSE_CLICKED && event.getButton() == MouseButton.PRIMARY)
            {
                trojkat.toFront();
                x = event.getX();
                y = event.getY();
            }
            if (event.getEventType() == MouseEvent.MOUSE_DRAGGED && event.getButton() == MouseButton.PRIMARY)
            {
                przesun(event);
            }
  
        }
    }

    /**
     * Klasa implementująca scrollowanie trójkąta
     */  
    class TrojkatScrollEventHandler implements EventHandler<ScrollEvent>
    {
        Trojkat trojkat;
        double x0;
        double y0;

        /**
         * Metoda odpowiedzialna za skalowanie trójkąta
         * @param event zdarzenie używania scrolla
         */
        private void przeskaluj(ScrollEvent event) 
        {
            double x = event.getX();
            double y = event.getY();
            /** Dynamiczna lista punktów trójkąta */
            ObservableList<Double> punkty = getPoints();
            
            //Jeśli nacisnęliśmy na trójkąt
            if (trojkat.czyTrafiony(x, y)) 
            {
                x0 = (punkty.get(0) + punkty.get(2) + punkty.get(4)) / 3;
                punkty.set(0, punkty.get(0) + event.getDeltaY() * 0.001 * (punkty.get(0) - x0));
                punkty.set(2, punkty.get(2) + event.getDeltaY() * 0.001 * (punkty.get(2) - x0));
                punkty.set(4, punkty.get(4) + event.getDeltaY() * 0.001 * (punkty.get(4) - x0));

                y0 = (punkty.get(0) + punkty.get(2) + punkty.get(4)) / 3.0;
                punkty.set(1, punkty.get(1) + event.getDeltaY() * 0.001 * (punkty.get(1) - y0));
                punkty.set(3, punkty.get(3) + event.getDeltaY() * 0.001 * (punkty.get(3) - y0));
                punkty.set(5, punkty.get(5) + event.getDeltaY() * 0.001 * (punkty.get(5) - y0));
            }
        }            

        /**
         * Nadpisana metoda handle wywołująca metodę skalowania koła
         * @param event zdarzenie używania scrolla
         * @see TrojkatScrollHandler#przeskaluj(ScrollEvent)
         */
        @Override
        public void handle(ScrollEvent event) 
        {  
            trojkat = (Trojkat) event.getSource();
            if (event.getEventType() == ScrollEvent.SCROLL)
            {
                przeskaluj(event);
            }
        }
    }
}
