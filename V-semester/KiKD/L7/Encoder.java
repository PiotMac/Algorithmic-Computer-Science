import java.io.FileInputStream;
import java.io.FileOutputStream;

public class Encoder {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Check <originalFile> <encodedFile>");
            return;
        }
        String originalFile = args[0];
        String encodedFile = args[1];

        try {
            FileInputStream fis = new FileInputStream(originalFile);
            FileOutputStream fos = new FileOutputStream(encodedFile);

            fis.close();
            fos.close();
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the first/second file!");
        }
    }
}
