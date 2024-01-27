import java.io.FileInputStream;
import java.io.FileOutputStream;

public class Decoder {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Check <encodedFile> <decodedFile>");
            return;
        }
        String encodedFile = args[0];
        String decodedFile = args[1];

        try {
            FileInputStream fis = new FileInputStream(encodedFile);
            FileOutputStream fos = new FileOutputStream(decodedFile);

            fis.close();
            fos.close();
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the first/second file!");
        }
    }
}
