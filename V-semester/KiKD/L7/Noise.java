import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.security.SecureRandom;
import java.util.ArrayList;

public class Noise {
    private static final SecureRandom secureRandom = new SecureRandom();

    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.println("Usage: java Noise <probability> <inputFile> <outputFile>");
            return;
        }
        double probability;
        String inputFile = args[1];
        String outputFile = args[2];
        try {
            probability = Double.parseDouble(args[0]);
            if (probability < 0.0 || probability > 1.0) {
                System.out.println("Probability must be between 0.0 and 1.0!");
                return;
            }
        }
        catch (NumberFormatException ex) {
            System.out.println("Wrong number format!");
            return;
        }
        try {
            FileInputStream fis = new FileInputStream(inputFile);
            FileInputStream checkSize = new FileInputStream(inputFile);
            FileOutputStream fos = new FileOutputStream(outputFile);

            int bitsRemaining = 0;
            int numberOfBytes = 0;

            while (checkSize.read() != -1) {
                numberOfBytes++;
            }

            int currentByte = 0;
            int currentBit = 0;
            ArrayList<Integer> finalResult = new ArrayList<>();

            for (int i = 0; i < numberOfBytes * 8; i++) {
                if (bitsRemaining == 0) {
                    currentByte = fis.read();
                    bitsRemaining = 8;
                }

                currentBit = (currentByte >> (bitsRemaining - 1)) & 1;
                bitsRemaining--;

                // Inverting the received bit with the given probability
                if (secureRandom.nextDouble() < probability) {
                    if (currentBit == 0) {
                        currentBit = 1;
                    }
                    else {
                        currentBit = 0;
                    }
                }

                finalResult.add(currentBit);
            }

            int bufferSize = 0;
            int buffer = 0;
            for (Integer bit : finalResult) {
                if (bufferSize == 8) {
                    fos.write(buffer);
                    bufferSize = 0;
                    buffer = 0;
                }
                buffer = (buffer << 1) | bit;
                bufferSize++;
            }

            if (bufferSize > 0) {
                buffer <<= (8 - bufferSize);
                fos.write(buffer);
            }


            fis.close();
            fos.close();
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the input/output file!");
        }
    }
}
