import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Main class of the quantization program
 */
public class QuantizationMain {
    /**
     * Default main method to run the program
     * @param args [0] - input file string, [1] - output file string, [2] - integer x between 0 - 24 describing 2<sup>x</sup> number of colors used in the quantization process
     */
    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.println("Usage: java QuantizationMain <inputFile> <outputFile> <0-24>");
            return;
        }
        String inputFile = args[0];
        String outputFile = args[1];
        int numberOfColors = 0;
        try {
            numberOfColors = Integer.parseInt(args[2]);
        }
        catch (NumberFormatException ex) {
            System.out.println("Third argument is not an integer!");
            return;
        }
        if (numberOfColors >= 0 && numberOfColors <= 24) {
            numberOfColors = 1 << numberOfColors;
        }
        else {
            System.out.println("Choose a number between 0 and 24.");
            return;
        }

        TgaReader tgaReader = new TgaReader();
        TgaWriter tgaWriter = new TgaWriter();
        Quantization quantization = new Quantization();
        TgaImage inputImage = new TgaImage();
        TgaImage quantizedImage = new TgaImage();

        try (FileInputStream fis = new FileInputStream(inputFile)) {
            FileOutputStream fos = new FileOutputStream(outputFile);

            // Read the TGA input image
            tgaReader.readTgaImage(inputImage, fis);

            // Clone the input image data
            cloneTgaImage(quantizedImage, inputImage);

            // Use quantization
            quantization.quantify(quantizedImage, numberOfColors);

            // Calculate proper values
            System.out.println("MSE = " + quantization.calculateMSE(inputImage, quantizedImage));
            System.out.println("SNR = " + quantization.calculateSNR(inputImage, quantizedImage) + "dB");

            // Write the quantized image to the file
            tgaWriter.writeTgaImage(quantizedImage, fos);
        }
        catch (IOException e) {
            System.out.println("File not found!");
        }
    }

    /**
     * Static method for copying data from source image into the destination image
     * @param destinationImage TgaImage to which the data is being copied
     * @param sourceImage TgaImage from which the data is being copied
     */
    private static void cloneTgaImage(TgaImage destinationImage, TgaImage sourceImage) {
        destinationImage.idField = sourceImage.idField;
        destinationImage.colorMapType = sourceImage.colorMapType;
        destinationImage.imageType = sourceImage.imageType;

        destinationImage.firstEntryIndex = sourceImage.firstEntryIndex;
        destinationImage.colorMapLength = sourceImage.colorMapLength;
        destinationImage.colorMapEntrySize = sourceImage.colorMapEntrySize;

        destinationImage.xOrigin = sourceImage.xOrigin;
        destinationImage.yOrigin = sourceImage.yOrigin;
        destinationImage.imageWidth = sourceImage.imageWidth;
        destinationImage.imageHeight = sourceImage.imageHeight;
        destinationImage.pixelDepth = sourceImage.pixelDepth;
        destinationImage.imageDescriptor = sourceImage.imageDescriptor;

        destinationImage.imageID.addAll(sourceImage.imageID);

        destinationImage.footer.addAll(sourceImage.footer);

        destinationImage.imagePixels = new int[sourceImage.imagePixels.length][sourceImage.imagePixels[0].length][3];

        for (int i = 0; i < sourceImage.imagePixels.length; i++) {
            for (int j = 0; j < sourceImage.imagePixels[0].length; j++) {
                for (int k = 0; k < 3; k++) {
                    destinationImage.imagePixels[i][j][k] = sourceImage.imagePixels[i][j][k];
                }
            }
        }
    }
}