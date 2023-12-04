import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

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
}
