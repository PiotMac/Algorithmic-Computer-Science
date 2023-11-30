import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length < 3 || args.length > 4) {
            System.out.println("Usage: java LZWCompression <encode|decode> [g|d|o|f] <input_file> <output_file>");
            return;
        }

        String mode = args[0].toLowerCase();
        String codingType = (args.length == 4) ? args[1].toLowerCase() : "omega";
        String inputFileName = (args.length == 4) ? args[2] : args[1];
        String outputFileName = (args.length == 4) ? args[3] : args[2];

        LZW lzw = new LZW();
        Coding coding;

        if (codingType.equals("o")) {
            coding = new OmegaCoding();
        }
        else if(codingType.equals(("g"))) {
            coding = new GammaCoding();
        }
        else if(codingType.equals(("d"))) {
            coding = new DeltaCoding();
        }
        else if(codingType.equals(("f"))) {
            coding = new FibonacciCoding();
        }
        else {
            System.out.println("Invalid coding type! Use 'g', 'd', 'o', 'f' or just don't use any flag and run the program using Gamma Coding by default.");
            return;
        }

        if (mode.equals("encode")) {
            List<Integer> encoded = lzw.encode(inputFileName);

            Map<Byte, Integer> frequencyMap = generateFrequencyMap(inputFileName, LZW.totalBytes);

            coding.encode(encoded, outputFileName);

            System.out.println("Size of the file BEFORE encoding: " + LZW.totalBytes + "B");
            System.out.println("Size of the file AFTER  encoding: " + Coding.encodedFileBytes + "B");
            System.out.println("Compression ratio               : " + 100.0 * ((double) Coding.encodedFileBytes / (double) LZW.totalBytes) + "%");
            System.out.println("Entropy BEFORE encoding         : " + calculateEntropy(frequencyMap, LZW.totalBytes));
            System.out.println("Entropy AFTER  encoding         : " + calculateCodeEntropy(encoded, LZW.totalBytes));
        } else if (mode.equals("decode")) {
            List<Integer> decoded = coding.decode(inputFileName);
            lzw.decode(decoded, outputFileName);
        } else {
            System.out.println("Invalid mode. Use 'encode' or 'decode'.");
            return;
        }
    }

    private static Map<Byte, Integer> generateFrequencyMap(String inputFileName, int totalBytes) {
        Map<Byte, Integer> frequencyMap = new HashMap<>();
        try (FileInputStream fis = new FileInputStream(inputFileName)) {
            int byteRead;
            while ((byteRead = fis.read()) != -1) {
                totalBytes++;
                frequencyMap.put((byte) byteRead, frequencyMap.getOrDefault((byte) byteRead, 0) + 1);
            }
        }
        catch (IOException e) {
            e.getMessage();
        }
        return frequencyMap;
    }
    private static double calculateEntropy(Map<Byte, Integer> frequencyMap, int totalBytes) {
        double entropy = 0.0;
        for (byte key : frequencyMap.keySet()) {
            double probability = (double) frequencyMap.get(key) / totalBytes;
            entropy -= probability * (Math.log(probability) / Math.log(2));
        }
        return entropy;
    }

    private static double calculateCodeEntropy(List<Integer> encodedData, int totalBytes) {
        HashMap<Integer, Integer> frequencyMap = new HashMap<>();
        for (int record : encodedData) {
            frequencyMap.put(record, frequencyMap.getOrDefault(record, 0) + 1);
        }

        double entropy = 0.0;
        for (int key : frequencyMap.keySet()) {
            double probability = (double) frequencyMap.get(key) / totalBytes;
            entropy -= probability * (Math.log(probability) / Math.log(2));
        }
        return entropy;
    }
}