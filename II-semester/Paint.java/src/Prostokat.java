import javafx.geometry.Bounds;
import javafx.event.EventHandler;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.ScrollEvent;
import javafx.scene.Cursor;
import javafx.scene.shape.Rectangle;
import javafx.scene.paint.Color;

/**
 * Klasa dziedzicząca po klasie Rectangle
 */
public class Prostokat extends Rectangle
{
    /**
     * Podstawowy kosntruktor
     * @param x współrzędna x lewego, górnego wierzchołka prostokąta
     * @param y współrzędna y lewego, górnego wierzchołka prostokąta
     * @param szerokosc współrzędna x prawego, dolnego wierzchołka prostokąta
     * @param wysokosc współrzędna y prawego, dolnego wierzchołka prostokąta
     */
    public Prostokat(double x, double y, double szerokosc, double wysokosc) 
    { 
        super(x, y, szerokosc, wysokosc);
        setFill(Color.RED);
        setOnMouseClicked(new ProstokatEventHandler());
        setOnMouseDragged(new ProstokatEventHandler());
        setOnMouseEntered(new ProstokatEventHandler());
        setOnMouseExited(new ProstokatEventHandler());
        setOnScroll(new ProstokatScrollEventHandler());
    }

    /**
     * Metoda sprawdzająca czy użytkownik najechał kursorem na prostokąt
     * @param x współrzędna x kursora na panelu
     * @param y współrzędna y kursora na panelu
     * @return tak lub nie w zależności od tego czy kursor najechał na prostokąt
     */
    private boolean czyTrafiony(double x, double y)
    {
        return getBoundsInLocal().contains(x,y);
    }

    /**
     * Klasa implementująca przesuwanie prostokąta
     */
    class ProstokatEventHandler implements EventHandler<MouseEvent>
    {
        Prostokat prostokat;
        private double x;
        private double y;
  
        /**
         * Metoda odpowiedzialna za przesuwanie prostokąta
         * @param event zdarzenie używania myszki
         */
        private void przesun(MouseEvent event) 
        {       
            double dx = event.getX() - x;
            double dy = event.getY() - y;
  
            //Jeśli nacisnęliśmy na prostokąt
            if (prostokat.czyTrafiony(x, y)) 
            {
                prostokat.setX(prostokat.getX()+ dx);
                prostokat.setY(prostokat.getY()+ dy);
            }
            //Aktualizacja pozycji kursora
            x += dx;
            y += dy;            
        }

        /**
         * Nadpisana metoda handle wywołująca metodę przesuwania prostokąta
         * @param event zdarzenie używania myszki
         * @see ProstokatEventHandler#przesun(MouseEvent)
         */
        @Override
        public void handle(MouseEvent event) 
        {
            prostokat = (Prostokat) event.getSource();
            ////Najeżdżając na prostokąt tworzy się obramowanie
            if (event.getEventType() == MouseEvent.MOUSE_ENTERED)
            {
                prostokat.setCursor(Cursor.HAND);
                prostokat.setStroke(Color.GREEN);
                prostokat.setStrokeWidth(8);
            }
            //Zdejmując kursor z prostokąta obramowanie znika
            if (event.getEventType() == MouseEvent.MOUSE_EXITED)
            {
                prostokat.setCursor(Cursor.DEFAULT);
                prostokat.setStrokeWidth(0);
            }
            if (event.getEventType() == MouseEvent.MOUSE_CLICKED && event.getButton() == MouseButton.PRIMARY)
            {
                prostokat.toFront();
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
     * Klasa implementująca skalowanie prostokąta
     */
    class ProstokatScrollEventHandler implements EventHandler<ScrollEvent>
    {
        Prostokat prostokat;

        /**
         * Metoda odpowiedzialna za skalowanie prostokąta
         * @param event zdarzenie używania scrolla
         */
        private void przeskaluj(ScrollEvent event) 
        {
            double x = event.getX();
            double y = event.getY();
  
            //Jeśli nacisnęliśmy na prostokąt
            if (prostokat.czyTrafiony(x, y)) 
            {                 
                prostokat.setWidth(prostokat.getWidth() + prostokat.getWidth() * event.getDeltaY() * 0.001);
                prostokat.setHeight(prostokat.getHeight() + prostokat.getHeight() * event.getDeltaY() * 0.001);
            }
        }            
  
        /**
         * Nadpisana metoda handle wywołująca metodę skalowania prostokąta
         * @param event zdarzenie używania scrolla
         * @see ProstokatScrollHandler#przeskaluj(ScrollEvent)
         */
        @Override
        public void handle(ScrollEvent event) 
        {  
            prostokat = (Prostokat) event.getSource();
            if (event.getEventType() == ScrollEvent.SCROLL)
            {
                przeskaluj(event);
            }
        }
    }
}