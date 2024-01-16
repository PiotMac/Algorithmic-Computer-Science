import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Class responsible for correct writing bytes into .tgs file
 */
public class TgaWriter {
    FileOutputStream fos;
    int numBitsFilled;
    int currentByte;

    public TgaWriter(FileOutputStream fos) {
        this.fos = fos;
        this.numBitsFilled = 0;
        this.currentByte = 0;
    }

    public void writeBits(int value, int numBits) throws IOException {
        for (int i = numBits - 1; i >= 0; i--) {
            int bit = (value >> i) & 1;
            currentByte = (currentByte << 1) | bit;
            numBitsFilled++;

            if (numBitsFilled == 8) {
                flush();
            }
        }
    }
    public void writeOneByte(int input) throws IOException {
        fos.write(input);
    }
    public void writeTwoBytes(int input) throws IOException {
        // Convert the int to two little-endian bytes
        byte byte0 = (byte) (input & 0xFF);
        byte byte1 = (byte) ((input >> 8) & 0xFF);

        // Write the two bytes to the output stream in little-endian order
        fos.write(byte0);
        fos.write(byte1);
    }

    public void writeRGB(int[] pixel) throws IOException {
        // Blue
        fos.write(pixel[2]);
        // Green
        fos.write(pixel[1]);
        // Red
        fos.write(pixel[0]);
    }

    public void close() throws IOException {
        flush();
        fos.close();
    }

    private void flush() throws IOException {
        if (numBitsFilled > 0) {
            currentByte <<= (8 - numBitsFilled);
            fos.write(currentByte);
            currentByte = 0;
            numBitsFilled = 0;
        }
    }

    public void writeTgaHeader(TgaImage quantizedImage) throws IOException {
        // -------------- HEADER PART (18 bytes) --------------
        // First part of header (3 bytes)
        writeOneByte(quantizedImage.idField);
        writeOneByte(quantizedImage.colorMapType);
        writeOneByte(quantizedImage.imageType);
        // Color map specification (5 bytes)
        writeTwoBytes(quantizedImage.firstEntryIndex);
        writeTwoBytes(quantizedImage.colorMapLength);
        writeOneByte(quantizedImage.colorMapEntrySize);
        // Image specification (10 bytes)
        writeTwoBytes(quantizedImage.xOrigin);
        writeTwoBytes(quantizedImage.yOrigin);
        writeTwoBytes(quantizedImage.imageWidth);
        writeTwoBytes(quantizedImage.imageHeight);
        writeOneByte(quantizedImage.pixelDepth);
        writeOneByte(quantizedImage.imageDescriptor);
    }

    public void writeTgaImageID(TgaImage quantizedImage) throws IOException {
        for (int i = 0; i < quantizedImage.imageID.size(); i++) {
            writeOneByte(quantizedImage.imageID.get(i));
        }
    }

    public void writeTgaFooter(TgaImage quantizedImage) throws IOException {
        // -------------- FOOTER PART (26 bytes) --------------
        for (int i = 0; i < quantizedImage.footer.size(); i++) {
            writeOneByte(quantizedImage.footer.get(i));
        }
    }

    public void writeTgaImage(TgaImage quantizedImage) throws IOException {
        writeTgaHeader(quantizedImage);

        writeTgaImageID(quantizedImage);

        // Write all pixels
        for (int i = 0; i < quantizedImage.imageHeight; i++) {
            for (int j = 0; j < quantizedImage.imageWidth; j++) {
                writeRGB(quantizedImage.imagePixels[i][j]);
            }
        }

        writeTgaFooter(quantizedImage);
    }
}
