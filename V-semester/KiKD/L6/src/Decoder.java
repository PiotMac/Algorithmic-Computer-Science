import java.io.IOException;

public class Decoder {
    public int[] addPixels(int[] firstPixel, int[] secondPixel) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = (firstPixel[i] + secondPixel[i]) % 256;
        }

        return result;
    }
    public void decode(TgaImage inputImage, TgaReader tgaReader, TgaWriter tgaWriter) throws IOException {
        tgaReader.readTgaHeader(inputImage);
        inputImage.quantizerSize = tgaReader.getOneByte();
        int quantizerSize = inputImage.quantizerSize;
        tgaReader.readTgaImageID(inputImage);

        tgaWriter.writeTgaHeader(inputImage);
        tgaWriter.writeTgaImageID(inputImage);

        int[] previousPixel = {0, 0, 0};
        inputImage.imagePixels = new int[inputImage.imageHeight][inputImage.imageWidth][3];

        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                int[] foundPixel = {0, 0, 0};

                for (int a = 2; a >= 0; a--) {
                    foundPixel[a] = tgaReader.readBits(quantizerSize) << (8 - quantizerSize);
                }

                previousPixel = addPixels(previousPixel, foundPixel);
                inputImage.imagePixels[i][j] = previousPixel;
                tgaWriter.writeRGB(previousPixel);
            }
        }

        tgaReader.readTgaFooter(inputImage);
        tgaWriter.writeTgaFooter(inputImage);
        tgaWriter.close();
    }
}
