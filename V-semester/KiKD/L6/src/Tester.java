import java.io.*;
import java.util.Objects;

public class Tester {
    public static final int QUANTIZER_MAX_SIZE = 7;
    public static void main(String[] args) throws IOException {
        String testDirectory = "/home/monek/IdeaProjects/KKD/L6/tests";
        String testBinaryDirectory = "/home/monek/IdeaProjects/KKD/L6/bin";
        String testOutputDirectory = "/home/monek/IdeaProjects/KKD/L6/outputs";
        File dir = new File(testDirectory);
        File[] directoryListing = dir.listFiles();

        Encoder encoder = new Encoder();
        Decoder decoder = new Decoder();


        if (directoryListing != null) {
            for (File child : directoryListing) {
                String fileNameWithoutExtension = child.getName().substring(0, child.getName().length() - 4);
                System.out.println("====================== TESTING: " + child.getName() + " ======================");
                for (int quantizerSize = 1; quantizerSize <= 7; quantizerSize++) {
                    System.out.println("====================== QUANTIZER SIZE: " + quantizerSize + " ======================");
                    FileInputStream originalFis = new FileInputStream(testDirectory + "/" + child.getName());
                    FileOutputStream binFos = new FileOutputStream(testBinaryDirectory + "/" + quantizerSize + fileNameWithoutExtension + ".bin");

                    TgaReader tgaReader = new TgaReader(originalFis);
                    TgaImage originalImage = new TgaImage();

                    tgaReader.readTgaImage(originalImage);
                    originalImage.quantizerSize = quantizerSize;

                    encoder.encode(originalImage, binFos);
                    tgaReader.close();

                    FileInputStream encodedBinFile = new FileInputStream(testBinaryDirectory + "/" + quantizerSize + fileNameWithoutExtension + ".bin");
                    FileOutputStream decodedFile = new FileOutputStream(testOutputDirectory + "/" + quantizerSize + child.getName());

                    TgaImage decodedImage = new TgaImage();

                    tgaReader = new TgaReader(encodedBinFile);
                    TgaWriter tgaWriter = new TgaWriter(decodedFile);

                    decoder.decode(decodedImage, tgaReader, tgaWriter);

                    Statistics statistics = new Statistics();

                    double redMSE = 0.0;
                    double greenMSE = 0.0;
                    double blueMSE = 0.0;
                    double allMSE = 0.0;

                    double redSNR = 0.0;
                    double greenSNR = 0.0;
                    double blueSNR = 0.0;
                    double allSNR = 0.0;

                    for (Colors color : Colors.values()) {
                        // MSE
                        switch (color) {
                            case RED -> redMSE = statistics.calculateMSE(originalImage, decodedImage, color);
                            case GREEN -> greenMSE = statistics.calculateMSE(originalImage, decodedImage, color);
                            case BLUE -> blueMSE = statistics.calculateMSE(originalImage, decodedImage, color);
                            case ALL -> allMSE = statistics.calculateMSE(originalImage, decodedImage, color);
                        }
                        // SNR
                        switch (color) {
                            case RED -> redSNR = statistics.calculateSNR(originalImage, decodedImage, color);
                            case GREEN -> greenSNR = statistics.calculateSNR(originalImage, decodedImage, color);
                            case BLUE -> blueSNR = statistics.calculateSNR(originalImage, decodedImage, color);
                            case ALL -> allSNR = statistics.calculateSNR(originalImage, decodedImage, color);
                        }
                    }

                    System.out.println("########### MSE ##########");
                    System.out.println("RED    =  " + redMSE);
                    System.out.println("GREEN  =  " + greenMSE);
                    System.out.println("BLUE   =  " + blueMSE);
                    System.out.println("ALL    =  " + allMSE);
                    System.out.println("########### SNR ##########");
                    System.out.println("RED    =  " + redSNR + " dB");
                    System.out.println("GREEN  =  " + greenSNR + " dB");
                    System.out.println("BLUE   =  " + blueSNR + " dB");
                    System.out.println("ALL    =  " + allSNR + " dB");
                }
            }
        }
    }
}
