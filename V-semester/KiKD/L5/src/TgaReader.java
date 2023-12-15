import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Class responsible for correct reading of a .tga file
 */
public class TgaReader {
    public int getOneByte(FileInputStream fis) throws IOException {
        return fis.read() & 0xFF;
    }
    public int getTwoBytes(FileInputStream fis) throws IOException {
        byte[] bytes = new byte[2];
        fis.read(bytes);
        return ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getShort();
    }

    public int[] readRGB(FileInputStream fis) throws IOException {
        int blue = fis.read() & 0xFF;
        int green = fis.read() & 0xFF;
        int red = fis.read() & 0xFF;

        // Return RGB values as an int array
        return new int[]{red, green, blue};
    }

    public void readTgaImage(TgaImage inputImage, FileInputStream fis) throws IOException {
        inputImage.idField = getOneByte(fis);
        inputImage.colorMapType = getOneByte(fis);
        inputImage.imageType = getOneByte(fis);
        // Color map specification (5 bytes)
        inputImage.firstEntryIndex = getTwoBytes(fis);
        inputImage.colorMapLength = getTwoBytes(fis);
        inputImage.colorMapEntrySize = getOneByte(fis);
        // Image specification (10 bytes)
        inputImage.xOrigin = getTwoBytes(fis);
        inputImage.yOrigin = getTwoBytes(fis);
        inputImage.imageWidth = getTwoBytes(fis);
        inputImage.imageHeight = getTwoBytes(fis);
        inputImage.pixelDepth = getOneByte(fis);
        inputImage.imageDescriptor = getOneByte(fis);

        // Read image ID
        for (int i = 0; i < inputImage.idField; i++) {
            inputImage.imageID.add(getOneByte(fis));
        }

        // Read all pixels
        inputImage.imagePixels = new int[inputImage.imageHeight][inputImage.imageWidth][3];
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                inputImage.imagePixels[i][j] = readRGB(fis);
            }
        }

        // -------------- FOOTER PART (26 bytes) --------------
        for (int i = 0; i < 26; i++) {
            inputImage.footer.add(getOneByte(fis));
        }
    }
}
