import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.HashMap;
import java.util.Map;

public class HuffmanDecoding {
    public static String encodedFile;
    public static String decodedFile;
    private static final String DICTIONARY_FILE = "dictionary.bin";
    private static byte EOF_BYTE;
    public static boolean isAvailableEOF = false;

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Użycie: java HuffmanDecoding encoded_file decoded_file");
            return;
        }

        encodedFile = args[0];
        decodedFile = args[1];

        try {
            // Compression
            decode();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    private static void decode() throws IOException {
        FileInputStream fis = new FileInputStream(encodedFile);
        FileInputStream fisDictionary = new FileInputStream(DICTIONARY_FILE);
        ObjectInputStream oisDictionary = new ObjectInputStream(fisDictionary);

        // Read the dictionary
        HashMap<Byte, String> huffmanCodes;
        try {
            isAvailableEOF = (boolean) oisDictionary.readObject();
            if (isAvailableEOF) {
                EOF_BYTE = (byte) oisDictionary.readObject();
            }
            huffmanCodes = (HashMap<Byte, String>) oisDictionary.readObject();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return; // Handle the exception appropriately
        }

        // Write the decompressed data
        FileOutputStream fos = new FileOutputStream(decodedFile);

        int bitBuffer;
        StringBuilder currentCode = new StringBuilder();

        while ((bitBuffer = fis.read()) != -1) {
            for (int i = 7; i >= 0; i--) {
                int bit = (bitBuffer >> i) & 1;

                currentCode.append(bit);

                // Check if the current code exists in the dictionary
                for (Map.Entry<Byte, String> entry : huffmanCodes.entrySet()) {
                    if (entry.getValue().contentEquals(currentCode)) {
                        byte decodedByte = entry.getKey();

                        // Check if the decoded byte is the EOF symbol
                        if (isAvailableEOF && decodedByte == EOF_BYTE) {
                            // Stop decompression
                            fos.close();
                            return;
                        }

                        fos.write(decodedByte);
                        currentCode = new StringBuilder();
                        break;  // Break out of the dictionary loop
                    }
                }
            }
        }

        fis.close();
        fos.close();
        oisDictionary.close();
        fisDictionary.close();
    }
}
