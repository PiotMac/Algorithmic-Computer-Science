import java.io.*;

/**
 * Class for testing the quantization on given examples
 */
public class QuantizationTesting {
    public final static int DIFFERENT_NUMBER_OF_COLORS = 7;
    public static void main(String[] args) throws IOException {
        String testDirectory = "/home/monek/IdeaProjects/KKD/L5/tests";
        String testOutputDirectory = "/home/monek/IdeaProjects/KKD/L5/output";
        File dir = new File(testDirectory);
        File[] directoryListing = dir.listFiles();
        int[] numberOfColorsTable = new int[DIFFERENT_NUMBER_OF_COLORS];
        for (int i = 0; i < DIFFERENT_NUMBER_OF_COLORS; i++) {
            numberOfColorsTable[i] = 2 * i;
        }
        FileWriter dataOutput = new FileWriter(testOutputDirectory + "/data.csv");
        dataOutput.write("Filename;NumberOfColors;MSE;SNR\n");
        if (directoryListing != null) {
            for (File child : directoryListing) {
                for (int i = 0; i < DIFFERENT_NUMBER_OF_COLORS; i++) {
                    System.out.println("TESTING: " + child.getName() + " with " + numberOfColorsTable[i] + " colors!");
                    try {
                        FileInputStream fis = new FileInputStream(testDirectory + "/" + child.getName());
                        FileOutputStream fos = new FileOutputStream(testOutputDirectory + "/" + numberOfColorsTable[i] + child.getName());

                        TgaReader tgaReader = new TgaReader();
                        TgaWriter tgaWriter = new TgaWriter();
                        Quantization quantization = new Quantization();
                        TgaImage inputImage = new TgaImage();
                        TgaImage quantizedImage = new TgaImage();

                        int numberOfColors = 1 << numberOfColorsTable[i];

                        // Read the TGA input image
                        tgaReader.readTgaImage(inputImage, fis);

                        // Clone the input image data
                        cloneTgaImage(quantizedImage, inputImage);

                        // Use quantization
                        quantization.quantify(quantizedImage, numberOfColors);

                        // Calculate proper values and print them to a csv file
                        dataOutput.write(child.getName() + ";"
                                + numberOfColorsTable[i] + ";"
                                + quantization.calculateMSE(inputImage, quantizedImage) + ";"
                                + quantization.calculateSNR(inputImage, quantizedImage) + "\n");

                        // Write the quantized image to the file
                        tgaWriter.writeTgaImage(quantizedImage, fos);



                    } catch (IOException e) {
                        System.out.println("I/O exception!");
                        return;
                    }
                }
            }
        }
        dataOutput.close();
    }
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
