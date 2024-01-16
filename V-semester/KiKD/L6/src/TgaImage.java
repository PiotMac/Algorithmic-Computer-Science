import java.util.ArrayList;

/**
 * Class of an .tga image
 */
public class TgaImage {
    // First part of header (3 bytes)
    int idField;
    int colorMapType;
    int imageType;
    // Color map specification (5 bytes)
    int firstEntryIndex;
    int colorMapLength;
    int colorMapEntrySize;

    // Image specification (10 bytes)
    int xOrigin;
    int yOrigin;
    int imageWidth;
    int imageHeight;
    int pixelDepth;
    int imageDescriptor;

    int quantizerSize;

    ArrayList<Integer> imageID;
    ArrayList<Integer> footer;

    // Image pixels in RGB
    int[][][] imagePixels;

    /**
     * Constructor initializing imageID and footer arrays
     */
    public TgaImage() {
        imageID = new ArrayList<>();
        footer = new ArrayList<>();
    }
}
