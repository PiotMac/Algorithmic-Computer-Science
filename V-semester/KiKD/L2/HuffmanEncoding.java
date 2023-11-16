import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.Set;


public class HuffmanEncoding {
    private static HashMap<Byte, String> huffmanCodes = new HashMap<>();
    public static String inputFile;
    public static String binaryFile;
    private static final String DICTIONARY_FILE = "dictionary.bin";
    private static byte EOF_BYTE;
    public static boolean isAvailableEOF = false;

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Użycie: java HuffmanEncoding input_file encoded_file");
            return;
        }

        inputFile = args[0];
        binaryFile = args[1];

        try {
            // Compression
            encode();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void encode() throws IOException {
        FileInputStream fis = new FileInputStream(inputFile);
        FileOutputStream fos = new FileOutputStream(binaryFile);
        FileOutputStream fosDictionary = new FileOutputStream(DICTIONARY_FILE);
        ObjectOutputStream oosDictionary = new ObjectOutputStream(fosDictionary);


        HashMap<Byte, Integer> frequencyMap = new HashMap<>();
        int totalBytes = 0;

        // Calculate the frequency for each symbol
        int byteRead;
        while ((byteRead = fis.read()) != -1) {
            totalBytes++;
            frequencyMap.put((byte) byteRead, frequencyMap.getOrDefault((byte) byteRead, 0) + 1);
        }
        if (frequencyMap.size() > 256) {
            throw new IllegalArgumentException("Wrong alphabet! The size of it is bigger than 256.");
        }

        // If there is space for EOF, set the flag to true
        // Otherwise, it is false
        isAvailableEOF = frequencyMap.size() < 256;

        // Build priority queue for the Huffman Tree
        PriorityQueue<HuffmanNode> priorityQueue = new PriorityQueue<>();
        for (byte key : frequencyMap.keySet()) {
            HuffmanNode node = new HuffmanNode();
            node.data = key;
            node.frequency = frequencyMap.get(key);
            node.left = null;
            node.right = null;
            priorityQueue.add(node);
        }

        // Create an EOF symbol
        if (isAvailableEOF) {
            Set<Byte> allByteValues = new HashSet<>();
            for (int i = 0; i <= 255; i++) {
                allByteValues.add((byte) i);
            }
            Set<Byte> usedByteValues = frequencyMap.keySet();
            // Find the remaining byte values
            Set<Byte> remainingByteValues = new HashSet<>(allByteValues);
            remainingByteValues.removeAll(usedByteValues);
            // Choose one byte value from the remaining set as EOF symbol
            EOF_BYTE = remainingByteValues.iterator().next();
            // Add the EOF to the priority queue
            HuffmanNode EOFnode = new HuffmanNode();
            EOFnode.data = EOF_BYTE;
            EOFnode.frequency = 1;
            EOFnode.left = null;
            EOFnode.right = null;
            priorityQueue.add(EOFnode);

            frequencyMap.put(EOF_BYTE, 1);
        }

        // Build a Huffman Tree
        HuffmanNode root = buildHuffmanTree(priorityQueue);

        // Generate Huffman codes
        generateHuffmanCodes(root, "", huffmanCodes);

        oosDictionary.writeObject(isAvailableEOF);
        if (isAvailableEOF) {
            oosDictionary.writeObject(EOF_BYTE);
        }
        // Save the dictionary to the dictionary file
        oosDictionary.writeObject(huffmanCodes);



        // Write encoded data to the file
        fis.close();
        fis = new FileInputStream(inputFile);

        int bitBuffer = 0;
        int bitCount = 0;
        while ((byteRead = fis.read()) != -1) {
            String code = huffmanCodes.get((byte) byteRead);
            for (char bit : code.toCharArray()) {
                bitBuffer <<= 1;
                bitBuffer |= (bit == '1') ? 1 : 0;
                bitCount++;

                if (bitCount == 8) {
                    fos.write(bitBuffer);
                    bitBuffer = 0;
                    bitCount = 0;
                }
            }
        }


        // No space for EOF
        // Insert a non-leaf code
        if (bitCount > 0 && !isAvailableEOF) {
            // Extract the longest code
            String longestCode = "";

            for (Byte key : huffmanCodes.keySet()) {
                String currentCode = huffmanCodes.get(key);
                if (currentCode.length() > longestCode.length()) {
                    longestCode = currentCode;
                }
            }

            int lengthOfPadding = 8 - bitCount;
            char[] charactersOfTheCode = longestCode.toCharArray();

            for (int i = 0; i < lengthOfPadding; i++) {
                bitBuffer <<= 1;
                char bit = charactersOfTheCode[i];
                bitBuffer |= (bit == '1') ? 1 : 0;
            }
            // Write it to the file
            fos.write(bitBuffer);
        }
        // Space for EOF
        // Insert EOF
        else if (bitCount > 0) {
            // Extracting EOF code
            String codeEOF = huffmanCodes.get(EOF_BYTE);
            int lengthOfEOFCode = codeEOF.length();
            char[] codeEOFArray = codeEOF.toCharArray();

            int lengthOfPadding = 8 - bitCount;
            // Writing a part of EOF code
            if (lengthOfEOFCode >= lengthOfPadding) {
                for (int i = 0; i < lengthOfPadding; i++) {
                    bitBuffer <<= 1;
                    char bit = codeEOFArray[i];
                    bitBuffer |= (bit == '1') ? 1 : 0;
                }
            }
            // Writing the whole EOF code and filling it up with ones
            else {
                for (int i = 0; i < lengthOfEOFCode; i++) {
                    // Writing whole EOF code
                    bitBuffer <<= 1;
                    char bit = codeEOFArray[i];
                    bitBuffer |= (bit == '1') ? 1 : 0;
                }
                for (int j = 0; j < lengthOfPadding - lengthOfEOFCode; j++) {
                    // Filling up with ones
                    bitBuffer <<= 1;
                    bitBuffer |= 1;
                }
            }


            fos.write(bitBuffer);
        }

        FileInputStream binaryFileInput = new FileInputStream(binaryFile);
        int binaryFileLength = 0;

        while ((byteRead = binaryFileInput.read()) != -1) {
            binaryFileLength++;
        }

        binaryFileInput.close();

        double entropy = calculateEntropy(frequencyMap, totalBytes);
        double averageLength = calculateAverageLength(huffmanCodes, frequencyMap, totalBytes);
        double compressionRatio = calculateCompressionRatio(totalBytes, binaryFileLength);

        System.out.println("Entropia: " + entropy);
        System.out.println("Średnia długość kodowania: " + averageLength);
        System.out.println("Stopień kompresji: " + compressionRatio);

        fis.close();
        oosDictionary.close();
        fos.close();
    }

    private static HuffmanNode buildHuffmanTree(PriorityQueue<HuffmanNode> priorityQueue) {
        while (priorityQueue.size() > 1) {
            HuffmanNode node1 = priorityQueue.poll();
            HuffmanNode node2 = priorityQueue.poll();

            HuffmanNode mergedNode = new HuffmanNode();
            mergedNode.frequency = node1.frequency + node2.frequency;
            mergedNode.left = node1;
            mergedNode.right = node2;

            priorityQueue.add(mergedNode);
        }

        return priorityQueue.poll();
    }

    private static void generateHuffmanCodes(HuffmanNode root, String code, HashMap<Byte, String> huffmanCodes) {
        // If it is a leaf
        if (root.left == null && root.right == null) {
            huffmanCodes.put(root.data, code);
            return;
        }

        // Go to the left child
        generateHuffmanCodes(root.left, code + "0", huffmanCodes);
        // Go to the right child
        generateHuffmanCodes(root.right, code + "1", huffmanCodes);
    }

    private static double calculateEntropy(HashMap<Byte, Integer> frequencyMap, int totalBytes) {
        double entropy = 0.0;
        for (byte key : frequencyMap.keySet()) {
            double probability = (double) frequencyMap.get(key) / totalBytes;
            entropy -= probability * (Math.log(probability) / Math.log(2));
        }
        return entropy;
    }

    private static double calculateAverageLength(HashMap<Byte, String> huffmanCodes, HashMap<Byte, Integer> frequencyMap, int totalBytes) {
        double codeLength = 0;
        double probability = 0;
        double result = 0;
        for (byte key : huffmanCodes.keySet()) {
            codeLength = huffmanCodes.get(key).length();
            probability = (double) frequencyMap.get(key) / totalBytes;
            result += probability * codeLength;
        }
        return result;
    }

    private static double calculateCompressionRatio(int originalSize, int compressedSize) {
        return (double) originalSize / compressedSize;
    }

}
