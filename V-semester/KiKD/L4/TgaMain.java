import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class TgaMain {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: java TgaMain <inputFile.tga>");
            return;
        }
        String inputFile = args[0];
        TgaReader tgaReader = new TgaReader();
        PixelOperations pixelOperations = new PixelOperations();
        Statistics statistics = new Statistics();

        try (FileInputStream fis = new FileInputStream(inputFile)) {
            // -------------- HEADER PART (18 bytes) --------------
            // First part of header (3 bytes)
            int idField = tgaReader.getOneByte(fis);
            int colorMapType = tgaReader.getOneByte(fis);
            int imageType = tgaReader.getOneByte(fis);
            // Color map specification (5 bytes)
            int firstEntryIndex = tgaReader.getTwoBytes(fis);
            int colorMapLength = tgaReader.getTwoBytes(fis);
            int colorMapEntrySize = tgaReader.getOneByte(fis);
            // Image specification (10 bytes)
            int xOrigin = tgaReader.getTwoBytes(fis);
            int yOrigin = tgaReader.getTwoBytes(fis);
            int imageWidth = tgaReader.getTwoBytes(fis);
            int imageHeight = tgaReader.getTwoBytes(fis);
            int pixelDepth = tgaReader.getOneByte(fis);
            int imageDescriptor = tgaReader.getOneByte(fis);

            // Create a two-line pixel buffer
            int[][][] buffer = new int[2][imageWidth+1][3];

            // Initialize the buffer with black pixels
            for (int i = 0; i < 2; i++) {
                for (int j = 0; j <= imageWidth; j++) {
                    buffer[i][j] = new int[]{0, 0, 0};  // Initialize with black pixels
                }
            }

            // Getting the first line of pixels
            for(int i = 1; i <= imageWidth; i++) {
                buffer[0][i] = tgaReader.readRGB(fis);
            }

            for(int j = 0; j < imageHeight; j++) {
                int[][] temp = buffer[0];
                buffer[0] = buffer[1];
                buffer[1] = temp;

                if (j != imageHeight - 1) {
                    for(int i = 1; i <= imageWidth; i++) {
                        buffer[0][i] = tgaReader.readRGB(fis);
                    }
                }
                else {
                    for(int i = 1; i <= imageWidth; i++) {
                        buffer[0][i] = new int[]{0, 0, 0};
                    }
                }

                for (int k = 1; k <= imageWidth; k++) {
                    int[] pixel = buffer[1][k];
                    int[] west = buffer[1][k - 1];
                    int[] north = buffer[0][k];
                    int[] northWest = buffer[0][k - 1];
                    for (Predictors c : Predictors.values()) {
                        if (c == Predictors.INPUT) {
                            statistics.countPixel(c.toString(), pixel);
                        }
                        else {
                            int[] predictionDifference = pixelOperations.subtractPixels(pixel, c.predict(pixel, west, north, northWest, pixelOperations));
                            statistics.countPixel(c.toString(), predictionDifference);
                        }
                    }
                }
            }

            HashMap<String, HashMap<String, HashMap<Integer, Integer>>> counters = statistics.getCounters();
            HashMap<String, HashMap<String, Integer>> pixels = statistics.getPixels();
            double optimalImage = Double.MAX_VALUE;
            double optimalRed = Double.MAX_VALUE;
            double optimalGreen = Double.MAX_VALUE;
            double optimalBlue = Double.MAX_VALUE;

            String optimalImageMethod = "";
            String optimalRedMethod = "";
            String optimalGreenMethod = "";
            String optimalBlueMethod = "";
            int size = imageHeight * imageWidth;

            // STATISTICS
            for (Predictors c : Predictors.values()) {
                HashMap<String, Integer> predictorPixels = pixels.get(c.toString());
                HashMap<Integer, Integer> red = counters.get(c.toString()).get("red");
                HashMap<Integer, Integer> green = counters.get(c.toString()).get("green");
                HashMap<Integer, Integer> blue = counters.get(c.toString()).get("blue");
                double imageAnswer = statistics.calculateEntropyOfPixels(predictorPixels, size);
                double redAnswer = statistics.calculateEntropy(red, size);
                double greenAnswer = statistics.calculateEntropy(green, size);
                double blueAnswer = statistics.calculateEntropy(blue, size);

                if (imageAnswer < optimalImage) {
                    optimalImage = imageAnswer;
                    optimalImageMethod = c.toString();
                }
                if (redAnswer < optimalRed) {
                    optimalRed = redAnswer;
                    optimalRedMethod = c.toString();
                }
                if (greenAnswer < optimalGreen) {
                    optimalGreen = greenAnswer;
                    optimalGreenMethod = c.toString();
                }
                if (blueAnswer < optimalBlue) {
                    optimalBlue = blueAnswer;
                    optimalBlueMethod = c.toString();
                }

                if (c == Predictors.INPUT) {
                    System.out.println("############## METHOD " + c + " ##############");
                    System.out.println("IMAGE ENTROPY        : " + imageAnswer);
                    System.out.println("RED ENTROPY          : " + redAnswer);
                    System.out.println("GREEN ENTROPY        : " + greenAnswer);
                    System.out.println("BLUE ENTROPY         : " + blueAnswer);
                }
                else {
                    System.out.println("############## METHOD " + c + " ##############");
                    System.out.println("IMAGE ENTROPY DIFF   : " + imageAnswer);
                    System.out.println("RED ENTROPY DIFF     : " + redAnswer);
                    System.out.println("GREEN ENTROPY DIFF   : " + greenAnswer);
                    System.out.println("BLUE ENTROPY DIFF    : " + blueAnswer);
                }
            }

            System.out.println("############## OPTIMAL METHODS ##############");
            System.out.println("OPTIMAL IMAGE ENTROPY DIFF    : " + optimalImage + "   " + optimalImageMethod);
            System.out.println("OPTIMAL RED ENTROPY DIFF      : " + optimalRed + "   " + optimalRedMethod);
            System.out.println("OPTIMAL GREEN ENTROPY DIFF    : " + optimalGreen + "   " + optimalGreenMethod);
            System.out.println("OPTIMAL BLUE ENTROPY DIFF     : " + optimalBlue + "   " + optimalBlueMethod);

        }
        catch (IOException e) {
            System.out.println("File not found!");
        }
    }
}