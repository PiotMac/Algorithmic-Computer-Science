import java.io.FileInputStream;
import java.io.IOException;

public class Statistics {
    /**
     * Function calculating mean square error
     * @param inputImage the input image instance
     * @param outputImage the quantized image instance
     * @param color certain color under consideration
     * @return mean square error of the two considering certain color (all whole image)
     */
    public double calculateMSE(TgaImage inputImage, TgaImage outputImage, Colors color) {
        double mse = 0.0;
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                switch (color) {
                    case RED -> mse += Math.pow((double) inputImage.imagePixels[i][j][0] - (double) outputImage.imagePixels[i][j][0], 2.0);
                    case GREEN -> mse += Math.pow((double) inputImage.imagePixels[i][j][1] - (double) outputImage.imagePixels[i][j][1], 2.0);
                    case BLUE -> mse += Math.pow((double) inputImage.imagePixels[i][j][2] - (double) outputImage.imagePixels[i][j][2], 2.0);
                    case ALL -> {
                        for (int k = 0; k < 3; k++) {
                            mse += Math.pow((double) inputImage.imagePixels[i][j][k] - (double) outputImage.imagePixels[i][j][k], 2.0);
                        }
                    }
                }
            }
        }

        return mse / ((double) inputImage.imageHeight * inputImage.imageWidth);
    }

    /**
     * Function calculating signal-to-noise ratio in dB
     * @param inputImage the input image instance
     * @param outputImage the quantized image instance
     * @param color certain color under consideration
     * @return signal-to-noise ratio of the two considering certain color (all whole image)
     */
    public double calculateSNR(TgaImage inputImage, TgaImage outputImage, Colors color) {
        double mse = calculateMSE(inputImage, outputImage, color);
        double snr = 0.0;
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                switch (color) {
                    case RED -> snr += Math.pow(inputImage.imagePixels[i][j][0], 2.0);
                    case GREEN -> snr += Math.pow(inputImage.imagePixels[i][j][1], 2.0);
                    case BLUE -> snr += Math.pow(inputImage.imagePixels[i][j][2], 2.0);
                    case ALL -> {
                        for (int k = 0; k < 3; k++) {
                            snr += Math.pow(inputImage.imagePixels[i][j][k], 2.0);
                        }
                    }
                }
            }
        }
        snr /= ((double) inputImage.imageHeight * inputImage.imageWidth);
        snr /= mse;

        snr = 10.0 * Math.log10(snr);

        return snr / mse;
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Statistics <originalImage> <decodedImage>");
            return;
        }
        String originalFile = args[0];
        String decodedFile = args[1];
        Statistics statistics = new Statistics();

        try (FileInputStream firstFis = new FileInputStream(originalFile)) {
            FileInputStream secondFis = new FileInputStream(decodedFile);
            TgaReader originalReader = new TgaReader(firstFis);
            TgaReader decodedReader = new TgaReader(secondFis);

            TgaImage originalImage = new TgaImage();
            TgaImage decodedImage = new TgaImage();

            originalReader.readTgaImage(originalImage);
            decodedReader.readTgaImage(decodedImage);

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

            System.out.println("################ MSE ###############");
            System.out.println("RED    =  " + redMSE);
            System.out.println("GREEN  =  " + greenMSE);
            System.out.println("BLUE   =  " + blueMSE);
            System.out.println("ALL    =  " + allMSE);
            System.out.println("################ SNR ###############");
            System.out.println("RED    =  " + redSNR + " dB");
            System.out.println("GREEN  =  " + greenSNR + " dB");
            System.out.println("BLUE   =  " + blueSNR + " dB");
            System.out.println("ALL    =  " + allSNR + " dB");

            originalReader.close();
            decodedReader.close();
        } catch (IOException e) {
            System.out.println("File not found!");
        }
    }
}
