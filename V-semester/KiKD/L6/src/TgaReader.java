import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Class responsible for correct reading of a .tga file
 */
public class TgaReader {
    FileInputStream fis;
    private int currentByte;
    private int bitsRemaining;

    public TgaReader(FileInputStream fis) {
        this.fis = fis;
        this.bitsRemaining = 0;
        this.currentByte = 0;
    }

    public int readBit() throws IOException {
        if (bitsRemaining == 0) {
            currentByte = fis.read();
            if (currentByte == -1) {
                return -1; // End of stream
            }
            bitsRemaining = 8;
        }

        int bit = (currentByte >> (bitsRemaining - 1)) & 1;
        bitsRemaining--;
        return bit;
    }

    public int readBits(int numBits) throws IOException {
        int result = 0;
        for (int i = 0; i < numBits; i++) {
            int bit = readBit();
            if (bit == -1) {
                return -1; // End of stream
            }
            result = (result << 1) | bit;
        }
        return result;
    }

    public int getOneByte() throws IOException {
        return fis.read() & 0xFF;
    }
    public int getTwoBytes() throws IOException {
        byte[] bytes = new byte[2];
        fis.read(bytes);
        return ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getShort();
    }

    public int[] readRGB() throws IOException {
        int blue = fis.read() & 0xFF;
        int green = fis.read() & 0xFF;
        int red = fis.read() & 0xFF;

        // Return RGB values as an int array
        return new int[]{red, green, blue};
    }

    public void close() throws IOException {
        fis.close();
    }

    public void readTgaHeader(TgaImage inputImage) throws IOException {
        inputImage.idField = getOneByte();
        inputImage.colorMapType = getOneByte();
        inputImage.imageType = getOneByte();
        // Color map specification (5 bytes)
        inputImage.firstEntryIndex = getTwoBytes();
        inputImage.colorMapLength = getTwoBytes();
        inputImage.colorMapEntrySize = getOneByte();
        // Image specification (10 bytes)
        inputImage.xOrigin = getTwoBytes();
        inputImage.yOrigin = getTwoBytes();
        inputImage.imageWidth = getTwoBytes();
        inputImage.imageHeight = getTwoBytes();
        inputImage.pixelDepth = getOneByte();
        inputImage.imageDescriptor = getOneByte();
    }

    public void readTgaImageID(TgaImage inputImage) throws IOException {
        // Read image ID
        for (int i = 0; i < inputImage.idField; i++) {
            inputImage.imageID.add(getOneByte());
        }
    }

    public void readTgaFooter(TgaImage inputImage) throws IOException {
        // -------------- FOOTER PART (26 bytes) --------------
        for (int i = 0; i < 26; i++) {
            inputImage.footer.add(getOneByte());
        }
    }

    public void readTgaImage(TgaImage inputImage) throws IOException {
        readTgaHeader(inputImage);

        readTgaImageID(inputImage);

        // Read all pixels
        inputImage.imagePixels = new int[inputImage.imageHeight][inputImage.imageWidth][3];
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                inputImage.imagePixels[i][j] = readRGB();
            }
        }

        readTgaFooter(inputImage);
    }
}
