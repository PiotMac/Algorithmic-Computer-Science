import javafx.event.EventHandler;
import javafx.scene.input.ScrollEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.Cursor;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.control.ColorPicker;
import javafx.event.ActionEvent;

/**
 * Klasa dziedzicząca po klasie Circle
 */
public class Kolo extends Circle
{
  /**
   * Podstawowy konstruktor
   * @param x współrzędna x środka okręgu
   * @param y współrzędna y środka okręgu
   * @param r promień okręgu
   */
  public Kolo(double x, double y, double r) 
  {
      super(x, y, r);
      setOnMouseClicked(new KoloEventHandler());
      setOnMouseDragged(new KoloEventHandler());
      setOnMouseEntered(new KoloEventHandler());
      setOnMouseExited(new KoloEventHandler());
      setOnScroll(new KoloScrollEventHandler());
  }
  /**
   * Metoda sprawdzająca czy użytkownik najechał kursorem na koło
   * @param x współrzędna x kursora na panelu
   * @param y współrzędna y kursora na panelu
   * @return tak lub nie w zależności od tego czy kursor najechał na koło
   */
  private boolean czyTrafiony(double x, double y)
  {
    return getBoundsInLocal().contains(x,y);
  }

  /**
   * Klasa implementująca scrollowanie koła
   */
  class KoloScrollEventHandler implements EventHandler<ScrollEvent>
  {
    Kolo kolo;

    /**
     * Metoda odpowiedzialna za skalowanie koła
     * @param event zdarzenie używania scrolla
     */
    private void przeskaluj(ScrollEvent event) 
    {          
      double x = event.getX();
      double y = event.getY();
  
      //Jeśli nacisnęliśmy na koło
      if (kolo.czyTrafiony(x, y)) 
      {
        kolo.setRadius(kolo.getRadius() + kolo.getRadius() * event.getDeltaY() * 0.001);
      }

    }            
    
    /**
     * Nadpisana metoda handle wywołująca metodę skalowania koła
     * @param event zdarzenie używania scrolla
     * @see KoloScrollHandler#przeskaluj(ScrollEvent)
     */
    @Override
    public void handle(ScrollEvent event) 
    { 
      kolo = (Kolo) event.getSource();
      if (event.getEventType() == ScrollEvent.SCROLL)
      {
        przeskaluj(event);
      }
    }
  }
  
  
  /**
   * Klasa implementująca przesuwanie koła
   */
  class KoloEventHandler implements EventHandler<MouseEvent>
  {
    Kolo kolo;
    private double x;
    private double y;
    
    /**
     * Metoda odpowiedzialna za przesuwanie koła
     * @param event zdarzenie używania myszki
     */
    private void przesun(MouseEvent event) 
    {          
      double dx = event.getX() - x;
      double dy = event.getY() - y;
  
      //Jeśli nacisnęliśmy na koło
      if (kolo.czyTrafiony(x, y)) 
      {
        kolo.setCenterX(kolo.getCenterX() + dx);
        kolo.setCenterY(kolo.getCenterY() + dy);
      }
      //Aktualizacja pozycji kursora
      x += dx;
      y += dy;            
    }
  
    /**
     * Nadpisana metoda handle wywołująca metodę przesuwania koła
     * @param event zdarzenie używania myszki
     * @see KoloEventHandler#przesun(MouseEvent)
     */
    @Override
    public void handle(MouseEvent event) 
    {
      kolo = (Kolo) event.getSource();
      //Najeżdżając na koło tworzy się obramowanie
      if (event.getEventType() == MouseEvent.MOUSE_ENTERED)
      {
        kolo.setCursor(Cursor.HAND);
        kolo.setStroke(Color.GREEN);
        kolo.setStrokeWidth(8);
      }
      //Zdejmując kursor z koła obramowanie znika
      if (event.getEventType() == MouseEvent.MOUSE_EXITED)
      {
        kolo.setCursor(Cursor.DEFAULT);
        kolo.setStrokeWidth(0);
      }
      if (event.getEventType() == MouseEvent.MOUSE_CLICKED && event.getButton() == MouseButton.PRIMARY)
      {
        kolo.toFront();
        x = event.getX();
        y = event.getY();
      }
      if (event.getEventType() == MouseEvent.MOUSE_DRAGGED && event.getButton() == MouseButton.PRIMARY)
      {
        przesun(event);
      }
    }
  }
}
