import javafx.scene.input.MouseEvent;
import javafx.scene.input.MouseButton;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Shape;
import javafx.scene.Group; 
import javafx.scene.shape.Rectangle;
import javafx.scene.transform.Rotate; 
import javafx.scene.transform.Scale; 
import javafx.scene.transform.Translate;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar.ButtonData;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.layout.HBox;
import javafx.scene.layout.BorderPane;
import javafx.scene.text.Text;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.*;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.ColorPicker;
import javafx.scene.Cursor; 

/**
 * Klasa programu odpowiedzialna za interfejs użytkownika oraz wywoływanie figur
 * @version 1.0
 * @author Piotr Maciejończyk
 */
public class Interfejs extends Application
{
    /**
     * Nadpisana metoda rozpoczęcia programu
     * @param stage główny panel
     */
    @Override
    public void start(Stage stage) 
    {
      //Obsługa okna dialogowego "info"
      Dialog<String> infoDialog = new Dialog<String>();
      infoDialog.setTitle("Info");
      ButtonType type = new ButtonType("OK", ButtonData.OK_DONE);
      infoDialog.setContentText("Nazwa programu: PAINt\nAutor: Piotr Maciejończyk\nPrzeznaczenie: Użytkownik może stworzyć na panelu okrąg, prostokąt oraz trójkąt wedle swojego uznania, a także jest w stanie te figury edytować.");
      infoDialog.getDialogPane().getButtonTypes().add(type);
      //Implementacja działania okna dialogowego "info"
      Button info = new Button("Info");
      info.setOnAction(new EventHandler <ActionEvent> ()
      {
        @Override
        public void handle(ActionEvent event)
        {
          infoDialog.showAndWait();
        }
      });

      //Obsługa okna dialogowego "instrukcja"
      Dialog<String> instrukcjaDialog = new Dialog<String>();
      instrukcjaDialog.setTitle("Instrukcja");
      instrukcjaDialog.setContentText("INSTRUKCJA!\n1. Rysowanie koła: Kliknij przycisk 'Koło', a następnie kliknij DWA razy LPM na panelu. W ten sposób tworzy się koło w miejscu pierwszego kliknięcia o promieniu drugiego kliknięcia.\n2. Rysowanie prostokąta: Kliknij przycisk 'Prostokąt', a następnie kliknij DWA razy LPM na panelu. W ten sposób tworzy się prostokąt o przekątnej wyznaczonej przez dwa kliknięcia.\n 3.Rysowanie trójkąta: Kliknij przycisk 'Trójkąt', a następnie kliknij TRZY razy LPM na panelu. W ten sposób tworzy się trójkąt o wierzchołkach w punktach, które kliknął użytkownik.\n\nTrzymając LPM, a następnie przeciągając kursorem po panelu można przesuwać figury, a za pomocą scrolla - powiększać i pomniejszać.");
      instrukcjaDialog.getDialogPane().getButtonTypes().add(type);
      //Implementacja działania okna dialogowego "instrukcja"
      Button instrukcja = new Button("Instrukcja");
      instrukcja.setOnAction(new EventHandler <ActionEvent> ()
      {
        @Override
        public void handle(ActionEvent event)
        {
          instrukcjaDialog.showAndWait();
        }
      });

      //Tworzenie interfejsu
      BorderPane paneBorder = new BorderPane();
      HBox paneHBox = new HBox(10);
      Pane pane = new Pane();
      paneBorder.setCenter(pane);

      //Tworzenie przycisków typu Toggle
      ToggleButton o = new ToggleButton("Okrąg");
      ToggleButton p = new ToggleButton("Prostokąt");
      ToggleButton t = new ToggleButton("Trójkąt");
      ToggleGroup tgl = new ToggleGroup();
      o.setToggleGroup(tgl);
      p.setToggleGroup(tgl);
      t.setToggleGroup(tgl);

      paneHBox.getChildren().addAll(o, p, t, info, instrukcja);
      paneBorder.setTop(paneHBox);

      //Tablica punktów, które wybiera użytkownik
      double[] punkty = new double[6];
      punkty[0] = -1;
      punkty[2] = -1;

      //Implementacja tworzenia figur na panelu
      pane.setOnMouseClicked(new EventHandler <MouseEvent>() 
      {
        @Override
        public void handle (MouseEvent event)
        {
          if(event.getEventType() == MouseEvent.MOUSE_CLICKED && event.getButton() == MouseButton.PRIMARY)
          {
            //Tworzenie okręgu
            if (o.isSelected())
            {
              if (punkty[0] == -1)
              {
                punkty[0] = event.getX();
                punkty[1] = event.getY();
              }
              else
              {
                punkty[2] = event.getX();
                punkty[3] = event.getY();
                double promien = Math.sqrt(Math.pow(punkty[2] - punkty[0], 2) + Math.pow(punkty[1] - punkty[3], 2));
                Kolo kolo = new Kolo(Math.min(punkty[0], punkty[2]), Math.min(punkty[1], punkty[3]), promien);
                pane.getChildren().add(kolo);

                punkty[0] = -1;
                punkty[2] = -1;
                tgl.selectToggle(null);
              }
            }

            //Tworzenie prostokąta
            if (p.isSelected())
            {
              if (punkty[0] == -1)
              {
                punkty[0] = event.getX();
                punkty[1] = event.getY();
              }
              else
              {
                punkty[2] = event.getX();
                punkty[3] = event.getY();
                double szerokosc = Math.abs(punkty[0] - punkty[2]);
                double wysokosc = Math.abs(punkty[1] - punkty[3]);
                Prostokat prostokat = new Prostokat(Math.min(punkty[0], punkty[2]), Math.min(punkty[1], punkty[3]), szerokosc, wysokosc);
                pane.getChildren().add(prostokat);

                punkty[0] = -1;
                punkty[2] = -1;
                tgl.selectToggle(null);
              }
            }

            //Tworzenie trójkąta
            if (t.isSelected())
            {
              if (punkty[0] == -1)
              {
                punkty[0] = event.getX();
                punkty[1] = event.getY();
              }
              else if (punkty[2] == -1)
              {
                punkty[2] = event.getX();
                punkty[3] = event.getY();
              }
              else
              {
                punkty[4] = event.getX();
                punkty[5] = event.getY();
                Trojkat trojkat = new Trojkat(punkty[0], punkty[1], punkty[2], punkty[3], punkty[4], punkty[5]);

                pane.getChildren().add(trojkat);

                punkty[0] = -1;
                punkty[2] = -1;
                tgl.selectToggle(null);
              }
            }
          }
        }
      });
 
      //Ustawienie sceny i stage'a
      Scene scene = new Scene(paneBorder, 600, 600, Color.BEIGE);
      stage.setTitle("PAINt");
      stage.setScene(scene);
      stage.show();
    }
    /**
     * Metoda main wywołująca program
     * @param args 
     */
    public static void main(String args[])
    {
       launch(args);
    }
 }