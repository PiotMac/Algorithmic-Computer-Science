import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;

public class Statistics {
    // Method, Component, Values, Frequencies
    private HashMap<String, HashMap<String, HashMap<Integer, Integer>>> counters;
    private HashMap<String, HashMap<String, Integer>> pixels;
    public Statistics() {
        counters = new HashMap<>();
        pixels = new HashMap<>();
        for (Predictors c : Predictors.values()) {
            HashMap<String, HashMap<Integer, Integer>> components = new HashMap<>();

            components.put("red", new HashMap<>());
            components.put("green", new HashMap<>());
            components.put("blue", new HashMap<>());

            counters.put(c.toString(), components);
            pixels.put(c.toString(), new HashMap<>());
        }
    }
    public void countPixel(String predictor, int[] pixel) {
        int red = pixel[0];
        int green = pixel[1];
        int blue = pixel[2];
        counters.get(predictor).get("red").put(red, counters.get(predictor).get("red").getOrDefault(red, 0) + 1);
        counters.get(predictor).get("green").put(green, counters.get(predictor).get("green").getOrDefault(green, 0) + 1);
        counters.get(predictor).get("blue").put(blue, counters.get(predictor).get("blue").getOrDefault(blue, 0) + 1);
        StringBuilder pixelToString = new StringBuilder();
        for (int i = 0; i < 2; i++) {
            pixelToString.append(pixel[i]);
            pixelToString.append(" ");
        }

        pixels.get(predictor).put(pixelToString.toString(), pixels.get(predictor).getOrDefault(pixelToString.toString(), 0) + 1);
    }

    public double calculateEntropyOfPixels(HashMap<String, Integer> frequencyMap, int totalSize) {
        double entropy = 0.0;
        for (String key : frequencyMap.keySet()) {
            double probability = (double) frequencyMap.get(key) / totalSize;
            entropy -= probability * (Math.log(probability) / Math.log(2));
        }
        return entropy;
    }

    public double calculateEntropy(HashMap<Integer, Integer> frequencyMap, int totalSize) {
        double entropy = 0.0;
        for (int key : frequencyMap.keySet()) {
            double probability = (double) frequencyMap.get(key) / totalSize;
            entropy -= probability * (Math.log(probability) / Math.log(2));
        }
        return entropy;
    }

    public HashMap<String, HashMap<String, HashMap<Integer, Integer>>> getCounters() {
        return counters;
    }

    public HashMap<String, HashMap<String, Integer>> getPixels() {
        return pixels;
    }
}
