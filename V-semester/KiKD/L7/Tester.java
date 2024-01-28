import java.security.SecureRandom;

public class Tester {
    private static final SecureRandom secureRandom = new SecureRandom();
    public static final int NO_BYTES_TO_ENCODE = 10000;
    public static final double PROBABILITY = 0.0001;

    public static void main(String[] args) throws Exception {
        int[] originalBits = new int[NO_BYTES_TO_ENCODE * 8];
        int[] encodedBits = new int[NO_BYTES_TO_ENCODE * 16];
        int[] changedBits = new int[NO_BYTES_TO_ENCODE * 16];
        int[] decodedBits = new int[NO_BYTES_TO_ENCODE * 8];
        // Initializing input
        for (int i = 0 ; i < NO_BYTES_TO_ENCODE * 8; i++) {
            originalBits[i] = secureRandom.nextInt(0, 2);
        }
        // Encoding and creating noise
        for (int i = 0; i < NO_BYTES_TO_ENCODE * 2; i++) {
            int[] bitsToChange = new int[4];
            for (int j = 0; j < 4; j++) {
                bitsToChange[j] = originalBits[4 * i + j];
            }
            int encodedByte = Encoder.encode4Bits(bitsToChange);
            for (int k = 0; k < 8; k++) {
                encodedBits[8 * i + k] = (encodedByte >> (7 - k)) & 0x01;
                if (secureRandom.nextDouble() < PROBABILITY) {
                    changedBits[8 * i + k] = encodedBits[8 * i + k] ^ 1;
                }
                else {
                    changedBits[8 * i + k] = encodedBits[8 * i + k];
                }
            }
        }
        // Calculating the difference
        int noDifferentFourSizedChunks = 0;
        for (int i = 0 ; i < NO_BYTES_TO_ENCODE * 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (encodedBits[4 * i + j] != changedBits[4 * i + j]) {
                    noDifferentFourSizedChunks++;
                    break;
                }
            }
        }
        System.out.println("Different 4-sized chunks = " + noDifferentFourSizedChunks);
        // Decoding
        int doubleErrors = 0;
        for (int i = 0; i < NO_BYTES_TO_ENCODE * 2; i++) {
            int[] receivedBits = new int[8];
            for (int j = 0; j < 8; j++) {
                receivedBits[j] = changedBits[8 * i + j];
            }
            int[] result = Decoder.decodeByte(receivedBits);
            if (result[4] == 1) {
                doubleErrors++;
            }
            for (int k = 0; k < 4; k++) {
                decodedBits[4 * i + k] = result[k];
            }
        }
        System.out.println("Double bit errors encountered = " + doubleErrors);
        double correctness = 0.0;
        for (int i = 0; i < NO_BYTES_TO_ENCODE * 8; i++) {
            if (originalBits[i] == decodedBits[i]) {
                correctness += 1.0;
            }
        }
        System.out.println("Correctness = " + (correctness / (NO_BYTES_TO_ENCODE * 8.0)) * 100.0 + "%");

    }
}
