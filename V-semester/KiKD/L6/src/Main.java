import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Objects;


public class Main {

    public static void main(String[] args) {
        if (args.length != 3 && args.length != 4) {
            System.out.println("Usage: java Main <encode | decode> <inputFile> <outputFile> <1-7 (in encoding)>");
            return;
        }
        String mode = args[0];
        String inputFile = args[1];
        String outputFile = args[2];
        if (Objects.equals(mode, "encode")) {
            int quantizerSize = 0;
            try {
                quantizerSize = Integer.parseInt(args[3]);
                if (quantizerSize < 1 || quantizerSize > 7) {
                    System.out.println("Choose a number between 1 and 7!");
                    return;
                }
                TgaImage inputImage = new TgaImage();
                Encoder encoder = new Encoder();

                try (FileInputStream fis = new FileInputStream(inputFile)) {
                    FileOutputStream fos = new FileOutputStream(outputFile);
                    TgaReader tgaReader = new TgaReader(fis);
                    // Read the TGA input image
                    tgaReader.readTgaImage(inputImage);
                    inputImage.quantizerSize = quantizerSize;

                    encoder.encode(inputImage, fos);
                    tgaReader.close();
                } catch (IOException e) {
                    System.out.println("File not found!");
                }

            } catch (NumberFormatException ex) {
                System.out.println("Third argument is not an integer!");
            }
        } else if (Objects.equals(mode, "decode")) {
            TgaImage inputImage = new TgaImage();
            Decoder decoder = new Decoder();

            try (FileInputStream fis = new FileInputStream(inputFile)) {
                FileOutputStream fos = new FileOutputStream(outputFile);
                TgaReader tgaReader = new TgaReader(fis);
                TgaWriter tgaWriter = new TgaWriter(fos);

                decoder.decode(inputImage, tgaReader, tgaWriter);
            } catch (IOException e) {
                System.out.println("File not found!");
            }
        } else {
            System.out.println("Write either 'encode' or 'decode'!");
        }
    }
}