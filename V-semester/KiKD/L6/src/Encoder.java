import java.io.FileOutputStream;
import java.io.IOException;

public class Encoder {
    public int[] addPixels(int[] firstPixel, int[] secondPixel) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = (firstPixel[i] + secondPixel[i]) % 256;
        }

        return result;
    }
    public void encode(TgaImage inputImage, FileOutputStream fos) throws IOException {
        int quantizerSize = inputImage.quantizerSize;

        TgaWriter tgaWriter = new TgaWriter(fos);
        tgaWriter.writeTgaHeader(inputImage);
        tgaWriter.writeOneByte(quantizerSize);
        tgaWriter.writeTgaImageID(inputImage);

        int[] previousPixel = {0, 0, 0};

        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                int[] currentPixel = inputImage.imagePixels[i][j];

                int[] quantizedDifference = new int[3];
                for (int k = 2; k >= 0; k--) {
                    quantizedDifference[k] = (currentPixel[k] - previousPixel[k]) % 256 >> (8 - quantizerSize);
                    tgaWriter.writeBits(quantizedDifference[k], quantizerSize);
                    quantizedDifference[k] = quantizedDifference[k] << (8 - quantizerSize);
                }

                previousPixel = addPixels(previousPixel, quantizedDifference);
            }
        }

        tgaWriter.writeTgaFooter(inputImage);
        tgaWriter.close();
    }
}
