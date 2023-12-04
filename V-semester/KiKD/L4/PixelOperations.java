public class PixelOperations {
    public int[] addPixels(int[] currentPixel, int[] prediction) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = (currentPixel[i] + prediction[i]) % 256;
        }
        return result;
    }

    public int[] subtractPixels(int[] currentPixel, int[] prediction) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = (currentPixel[i] - prediction[i] + 256) % 256;
        }
        return result;
    }

    public int[] scalePixel(int[] chosenPixel, int factor) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = (chosenPixel[i] / factor) % 256;
        }
        return result;
    }

}
