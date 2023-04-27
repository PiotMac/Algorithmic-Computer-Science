import java.awt.*;
import java.awt.event.*;
import java.io.*;

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
        super("Wypisz wiersz");
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
        super("Stwórz wiersz trójkąta Pascala");
        setBounds(100, 100, 300, 300);

        addWindowListener(new MyWindowAdapter());
        setFont(new Font(Font.SANS_SERIF,Font.PLAIN,30));
        setLayout(new GridLayout(3, 1));

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
            wiad.setAlignment(Label.CENTER);

            int n = Integer.parseInt(wiersz.getText());
            if(n >= 0 && n <= 30) 
            {
                wiad.setFont(new Font(Font.SANS_SERIF,Font.PLAIN,10));

                String command = "./main.out " + n;
                for(int i = 0; i <= n; i++) 
                {
                    command = command + " " + i;
                }
                Process process = Runtime.getRuntime().exec(command);

                BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));

                String wiersz1 = "", line;
                while((line = reader.readLine()) != null) 
                {
                    wiersz1 += line + " ";
                }
                reader.close();
                
                if (n < 5)
                {
                    this.setBounds(100, 100, 250, 250);
                    wiad.setText(wiersz1);
                } 
                else
                {
                    this.setBounds(100, 100, n * 50, 250);
                    wiad.setText(wiersz1);
                }
            }
            else 
            {
                wiad.setText("Nieprawidłowy numer wiersza!");
                this.setBounds(100, 100, 600, 250);
            }
        }
        catch (NumberFormatException ex) 
        {
            wiad.setText("Nieprawidłowy format wiersza!");
            this.setBounds(100, 100, 600, 250);
        }
        catch (IOException x) 
        {

        }
        wiersz.setText("");
    }
}

public class PascalCPP 
{
    public static void main(String args[]) 
    {
        MyFrame frame = new MyFrame();
        frame.setVisible(true);
    }    
}
