import java.awt.*;
import java.awt.event.*;

class MyWindowAdapter extends WindowAdapter 
{
    public void windowClosing(WindowEvent e) 
    {
        System.exit(0);
    }
}

class MyButton extends Button 
{
    MyButton(MyFrame f) 
    {
        super("Stwórz trójkąt");
        addActionListener(new MyButtonAdapter(f));
    }
}

class MyButtonAdapter implements ActionListener 
{
    MyFrame f;
    MyButtonAdapter(MyFrame f) 
    {
        this.f = f;
    }
    public void actionPerformed(ActionEvent e) 
    {
        f.action();
    }
}

class MyFrame extends Frame 
{
    TextField wiersz;
    MyButton wypisz;
    Label wiad;

    MyFrame () 
    {
        super("Stwórz trojkąt Pascala");
        setBounds(0, 0, 500, 250);

        addWindowListener(new MyWindowAdapter());
        setFont(new Font(Font.SANS_SERIF,Font.PLAIN,30));
        setLayout(new GridLayout(0, 1));

        wiersz = new TextField(50);
        wypisz = new MyButton(this);
        wiad = new Label();

        add(wiersz);
        add(wypisz);
        add(wiad);
    }

    public void action() 
    {
        try {
            int n = Integer.parseInt(wiersz.getText());
            if(n < 0 || n > 31) 
            {
                wiad.setText("Nieprawidłowy numer wiersza!");
            }
            else 
            {
                PascalFrame frame2 = new PascalFrame(n);
                this.setVisible(false);
                frame2.setVisible(true);
            }
        }
        catch (NumberFormatException ex) 
        {
            wiad.setText("Nieprawidłowy format wiersza!");
        }
        wiersz.setText("");
    }
}

class PascalFrame extends Frame 
{

    PascalFrame(int ilosc) 
    {
        super("Trójkąt Pascala");
        setBounds(0, 0, (ilosc + 1) * 80, ilosc * 60);

        addWindowListener(new MyWindowAdapter());
        setFont(new Font(Font.SANS_SERIF,Font.PLAIN,10));
        setLayout(new GridLayout(0, 1));

        for(int i = 0; i <= ilosc; i++) 
        {
            Label wiersze = new Label();
            add(wiersze);
            wiersze.setAlignment(Label.CENTER);
            wiersze.setText(Wiersz(i));
        }
    }
    
    private int element (int wiersz_e, int kolumna_e) 
    {
        if(kolumna_e == 0) 
        {
            return 1;
        }
        else if (kolumna_e == wiersz_e) 
        {
            return 1;
        }
        return (element(wiersz_e - 1, kolumna_e) + element(wiersz_e - 1, kolumna_e - 1));
    }

    private String Wiersz (int wiersz_w) 
    {
        String linia = "";
        for(int k = 0; k <= wiersz_w; k++) 
        {
            linia += element(wiersz_w, k);
            if(k != wiersz_w) 
            {
                linia += "  ";
            }
        }
        return linia;
    }
}

public class PascalGUI 
{
    public static void main(String args[]) 
    {
        MyFrame frame = new MyFrame();
        frame.setVisible(true);
    }
}
