import java.util.ArrayList;

/**
 * Class responsible for quantization of the image and equipped with methods to calculate mean square error
 * and signal-to-noise ratio
 */
public class Quantization {
    public final static double EPSILON = 0.001;
    /**
     * Function calculating mean square error
     * @param inputImage the input image instance
     * @param outputImage the quantized image instance
     * @return mean square error of the two
     */
    public double calculateMSE(TgaImage inputImage, TgaImage outputImage) {
        double mse = 0.0;
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                for (int k = 0; k < 3; k++) {
                    mse += Math.pow((double) inputImage.imagePixels[i][j][k] - (double) outputImage.imagePixels[i][j][k], 2.0);
                }
            }
        }

        return mse / ((double) inputImage.imageHeight * inputImage.imageWidth);
    }

    /**
     * Function calculating signal-to-noise ratio in dB
     * @param inputImage the input image instance
     * @param outputImage the quantized image instance
     * @return signal-to-noise ratio of the two
     */
    public double calculateSNR(TgaImage inputImage, TgaImage outputImage) {
        double mse = calculateMSE(inputImage, outputImage);
        double snr = 0.0;
        for (int i = 0; i < inputImage.imageHeight; i++) {
            for (int j = 0; j < inputImage.imageWidth; j++) {
                for (int k = 0; k < 3; k++) {
                    snr += Math.pow(inputImage.imagePixels[i][j][k], 2.0);
                }
            }
        }
        snr /= ((double) inputImage.imageHeight * inputImage.imageWidth);
        snr /= mse;

        snr = 10.0 * Math.log10(snr);

        return snr / mse;
    }

    /**
     * Function calculating color distance using taxi metrics in terms of RGB components
     * @param firstPixel array of RGB integer values of the first pixel
     * @param secondPixel array of RGB integer values of the second pixel
     * @return Difference between those pixels
     */
    private int taxiColorDistance(int[] firstPixel, int[] secondPixel) {
        int result = 0;
        for (int i = 0; i < 3; i++) {
            result += Math.abs(firstPixel[i] - secondPixel[i]);
        }

        return result;
    }

    /**
     * Function calculating average RGB value of the image
     * @param vectorColorMap Vector of all pixels of the image
     * @return Average RGB value of all the pixels
     */
    private int[] calculateAverageColor(int[][] vectorColorMap) {

        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            for (int[] pixel : vectorColorMap) {
                result[i] += pixel[i];
            }
            result[i] = result[i] / vectorColorMap.length;
        }
        // Red, Green, Blue
        return result;
    }

    /**
     * Function calculating average color of centroids
     * @param vectorColorMap array list of vectors of centroid pixels
     * @return average RGB values of the centroids
     */
    private int[] calculateAverageColor(ArrayList<int[]> vectorColorMap) {

        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            for (int[] pixel : vectorColorMap) {
                result[i] += pixel[i];
            }
            result[i] = result[i] / vectorColorMap.size();
        }
        // Red, Green, Blue
        return result;
    }

    /**
     * Method responsible for image quantization
     * @param quantizedImage TGA image to be quantized
     * @param numberOfColors number of colors to be used in the quantization process
     */
    public void quantify(TgaImage quantizedImage, int numberOfColors) {
        // Creating vector color map
        int[][] vectorColorMap = createVectorColorMap(quantizedImage.imagePixels);
        // Creating centroids
        ArrayList<int[]> centroids = createNeededCentroids(vectorColorMap, numberOfColors);

        for (int i = 0; i < quantizedImage.imageHeight; i++) {
            for (int j = 0; j < quantizedImage.imageWidth; j++) {
                int nearestCentroidDistance = taxiColorDistance(quantizedImage.imagePixels[i][j], centroids.get(0));
                int nearestCentroidIndex = 0;
                for (int centroidIndex = 1; centroidIndex < centroids.size(); centroidIndex++) {
                    int distance = taxiColorDistance(quantizedImage.imagePixels[i][j], centroids.get(centroidIndex));
                    if (distance < nearestCentroidDistance) {
                        nearestCentroidIndex = centroidIndex;
                        nearestCentroidDistance = distance;
                    }
                }
                quantizedImage.imagePixels[i][j] = centroids.get(nearestCentroidIndex);
            }
        }
    }

    /**
     * A function that creates the final list of centroids of the image
     * @param vectorColorMap image pixels
     * @param numberOfColors number of levels of the quantization
     * @return complete list of centroids of the image
     */
    private ArrayList<int[]> createNeededCentroids(int[][] vectorColorMap, int numberOfColors) {
        ArrayList<int[]> centroids = new ArrayList<>();
        // Linde-Buzo-Gray algorithm initialization
        int[] averagePixel = calculateAverageColor(vectorColorMap);
        // Perturbation value
        double averageDistortion = calculateAverageColorDistortion(vectorColorMap, averagePixel);
        // Beginning with the 1-element quantization dictionary
        // that consists of the average RGB values of all the pixels
        centroids.add(averagePixel);

        while (centroids.size() < numberOfColors) {
            // To each dictionary point we add the perturbation
            averageDistortion = refineCentroids(vectorColorMap, centroids, averageDistortion);
        }

        return centroids;
    }

    /**
     * A function that refines the centroids until convergence and calculates updated average distortion value
     * @param vectorColorMap image pixels
     * @param centroids current centroids
     * @param distortion previous average distortion value
     * @return updated average distortion value
     */
    private double refineCentroids(int[][] vectorColorMap, ArrayList<int[]> centroids, double distortion) {
        ArrayList<int[]> newCentroids = new ArrayList<>();
        for (int[] color : centroids) {
            // Step 1: Split each centroid into two near colors
            int firstRed = (color[0] == 255) ? color[0] : color[0] + 1;
            int secondRed = (color[0] == 0) ? color[0] : color[0] - 1;
            int firstGreen = (color[1] == 255) ? color[1] : color[1] + 1;
            int secondGreen = (color[1] == 0) ? color[1] : color[1] - 1;
            int firstBlue = (color[2] == 255) ? color[2] : color[2] + 1;
            int secondBlue = (color[2] == 0) ? color[2] : color[2] - 1;

            int[] newColor1 = new int[]{firstRed, firstGreen, firstBlue};
            int[] newColor2 = new int[]{secondRed, secondGreen, secondBlue};
            newCentroids.add(newColor1);
            newCentroids.add(newColor2);
        }

        double relativeError = 1.0 + EPSILON;
        double averageDistortion = 0.0;

        // Step 2: Iteratively refine centroids until convergence
        while (relativeError > EPSILON) {
            int[] nearestCentroidsIndexes = new int[vectorColorMap.length];
            ArrayList<Integer>[] pixelsForNearestCentroids = new ArrayList[newCentroids.size()];
            for (int i = 0; i < newCentroids.size(); i++) {
                pixelsForNearestCentroids[i] = new ArrayList<>();
            }

            // Step 3: Find nearest centroids for each pixel
            for (int pixelIndex = 0; pixelIndex < vectorColorMap.length; pixelIndex++) {
                int nearestCentroidIndex = 0;
                int nearestCentroidDistance = taxiColorDistance(vectorColorMap[pixelIndex], newCentroids.get(0));

                for (int centroidIndex = 1; centroidIndex < newCentroids.size(); centroidIndex++) {
                    int distance = taxiColorDistance(vectorColorMap[pixelIndex], newCentroids.get(centroidIndex));
                    if (distance < nearestCentroidDistance) {
                        nearestCentroidIndex = centroidIndex;
                        nearestCentroidDistance = distance;
                    }
                }

                nearestCentroidsIndexes[pixelIndex] = nearestCentroidIndex;
                pixelsForNearestCentroids[nearestCentroidIndex].add(pixelIndex);
            }

            // Step 4: Update centroids based on assigned pixels
            for (int centroidIndex = 0; centroidIndex < newCentroids.size(); centroidIndex++) {
                if (!pixelsForNearestCentroids[centroidIndex].isEmpty()) {
                    ArrayList<int[]> nearestPixels = new ArrayList<>();
                    for (int pixelIndex : pixelsForNearestCentroids[centroidIndex]) {
                        nearestPixels.add(vectorColorMap[pixelIndex]);
                    }
                    newCentroids.set(centroidIndex, calculateAverageColor(nearestPixels));
                }
            }

            // Step 5: Calculate average distortion and relative error
            ArrayList<int[]> closestCentroids = new ArrayList<>();
            for (int centroidIndex : nearestCentroidsIndexes) {
                closestCentroids.add(newCentroids.get(centroidIndex));
            }

            double previousAverageDistortion = (averageDistortion > 0) ? averageDistortion : distortion;
            averageDistortion = calculateAverageColorDistortion(vectorColorMap, closestCentroids);

            relativeError = Math.abs((previousAverageDistortion - averageDistortion) / previousAverageDistortion);
        }

        // Step 6: Update the original centroids and return the final average distortion
        centroids.clear();
        centroids.addAll(newCentroids);
        return averageDistortion;
    }

    /**
     * A function that calculates the average distortion of the image with respect to the closest centroids
     * @param vectorColorMap image pixels
     * @param averagePixel RGB values of the centroids
     * @return average distortion of the image with respect to the closest centroids
     */
    private double calculateAverageColorDistortion(int[][] vectorColorMap, ArrayList<int[]> averagePixel) {
        double averageDistorion = 0.0;
        for (int i = 0; i < vectorColorMap.length; i++) {
            averageDistorion += taxiColorDistance(vectorColorMap[i], averagePixel.get(i));
        }

        return averageDistorion / vectorColorMap.length;
    }

    /**
     * Function calculating the initial average color distortion among the image pixels
     * @param vectorColorMap vector of pixels
     * @param averagePixel pixel containing average RGB values of the pixels
     * @return average difference between an image pixel and the average pixel
     */
    private double calculateAverageColorDistortion(int[][] vectorColorMap, int[] averagePixel) {
        double averageDistorion = 0.0;
        for (int[] pixel : vectorColorMap) {
            averageDistorion += taxiColorDistance(pixel, averagePixel);
        }

        return averageDistorion / vectorColorMap.length;
    }

    /**
     * Function responsible for creating a vector of the image pixels
     * @param imagePixels 3-dimensional table storing the image pixels
     * @return 2-dimensional table storing the image pixels
     */
    private int[][] createVectorColorMap(int[][][] imagePixels) {
        int[][] vectorColorMap = new int[imagePixels.length * imagePixels[0].length][3];
        for (int i = 0; i < imagePixels.length; i++) {
            for (int j = 0; j < imagePixels[0].length; j++) {
                vectorColorMap[i * imagePixels[0].length + j] = imagePixels[i][j];
            }
        }

        return vectorColorMap;
    }
}
