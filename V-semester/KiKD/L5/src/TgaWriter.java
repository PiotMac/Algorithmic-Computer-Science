import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Class responsible for correct writing bytes into .tgs file
 */
public class TgaWriter {
    public void writeOneByte(FileOutputStream fos, int input) throws IOException {
        fos.write(input);
    }
    public void writeTwoBytes(FileOutputStream fos, int input) throws IOException {
        // Convert the int to two little-endian bytes
        byte byte0 = (byte) (input & 0xFF);
        byte byte1 = (byte) ((input >> 8) & 0xFF);

        // Write the two bytes to the output stream in little-endian order
        fos.write(byte0);
        fos.write(byte1);
    }

    public void writeRGB(FileOutputStream fos, int[] pixel) throws IOException {
        // Blue
        fos.write(pixel[2]);
        // Green
        fos.write(pixel[1]);
        // Red
        fos.write(pixel[0]);
    }

    public void writeTgaImage(TgaImage quantizedImage, FileOutputStream fos) throws IOException {
        // -------------- HEADER PART (18 bytes) --------------
        // First part of header (3 bytes)
        writeOneByte(fos, quantizedImage.idField);
        writeOneByte(fos, quantizedImage.colorMapType);
        writeOneByte(fos, quantizedImage.imageType);
        // Color map specification (5 bytes)
        writeTwoBytes(fos, quantizedImage.firstEntryIndex);
        writeTwoBytes(fos, quantizedImage.colorMapLength);
        writeOneByte(fos, quantizedImage.colorMapEntrySize);
        // Image specification (10 bytes)
        writeTwoBytes(fos, quantizedImage.xOrigin);
        writeTwoBytes(fos, quantizedImage.yOrigin);
        writeTwoBytes(fos, quantizedImage.imageWidth);
        writeTwoBytes(fos, quantizedImage.imageHeight);
        writeOneByte(fos, quantizedImage.pixelDepth);
        writeOneByte(fos, quantizedImage.imageDescriptor);

        // Write image ID
        for (int i = 0; i < quantizedImage.imageID.size(); i++) {
            writeOneByte(fos, quantizedImage.imageID.get(i));
        }

        // Write all pixels
        for (int i = 0; i < quantizedImage.imageHeight; i++) {
            for (int j = 0; j < quantizedImage.imageWidth; j++) {
                writeRGB(fos, quantizedImage.imagePixels[i][j]);
            }
        }

        // -------------- FOOTER PART (26 bytes) --------------
        for (int i = 0; i < quantizedImage.footer.size(); i++) {
            writeOneByte(fos, quantizedImage.footer.get(i));
        }
    }
}
