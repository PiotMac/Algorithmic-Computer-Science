import java.io.FileInputStream;
import java.io.FileOutputStream;

public class Decoder {
    public static int[] decodeByte(int[] dataBits) throws Exception {
        int[][] parityMatrix = {
                {0, 0, 1, 0, 1, 1, 1},
                {0, 1, 0, 1, 1, 1, 0},
                {1, 0, 1, 1, 1, 0, 0}
        };

        if (dataBits.length != 8) {
            throw new Exception("Eight bits at a time have to be passed to the decoder!");
        }

        int[] parityBits = new int[4];

        for (int i = 0; i < 4; i++) {
            parityBits[i] = 0;
            for (int j = 0; j < 7; j++) {
                if (i == 3) {
                    parityBits[i] = (parityBits[i] + dataBits[j]) % 2;
                }
                else if (parityMatrix[i][j] == 1) {
                    parityBits[i] = (parityBits[i] + dataBits[j]) % 2;
                }
            }
        }

        int wasUncorrected = 0;

        boolean isSyndromeAllZeros = parityBits[0] == 0 && parityBits[0] == parityBits[1] && parityBits[1] == parityBits[2];

        if (dataBits[7] == parityBits[3]) {
            if (!(isSyndromeAllZeros)) {
                wasUncorrected++;
            }
        }
        else {
            if (isSyndromeAllZeros) {
                dataBits[7] ^= 1;
            }
            else {
                for (int i = 0; i < 7; i++) {
                    if (parityMatrix[0][i] == parityBits[0] && parityMatrix[1][i] == parityBits[1] && parityMatrix[2][i] == parityBits[2]) {
                        dataBits[i] ^= 1;
                        break;
                    }
                }
            }
            /*
            for (int i = 0; i < 4; i++) {
                parityBits[i] = 0;
                for (int j = 0; j < 7; j++) {
                    if (i == 3) {
                        parityBits[i] = (parityBits[i] + dataBits[j]) % 2;
                    }
                    else if (parityMatrix[i][j] == 1) {
                        parityBits[i] = (parityBits[i] + dataBits[j]) % 2;
                    }
                }
            }
            isSyndromeAllZeros = parityBits[0] == 0 && parityBits[0] == parityBits[1] && parityBits[1] == parityBits[2];

            if (!isSyndromeAllZeros) {
                wasUncorrected++;
            }

             */
        }

        return new int[]{dataBits[0], dataBits[0] ^ dataBits[1], dataBits[5], dataBits[6], wasUncorrected};
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Decoder <encodedFile> <decodedFile>");
            return;
        }
        String encodedFile = args[0];
        String decodedFile = args[1];
        int uncorrectedErrors = 0;

        try {
            FileInputStream fis = new FileInputStream(encodedFile);
            FileOutputStream fos = new FileOutputStream(decodedFile);

            int byteRead;
            int iteration = 0;
            int decodedByte = 0;
            int bitsToMove = 7;
            while ((byteRead = fis.read()) != -1) {
                iteration++;

                int[] receivedBits = new int[8];
                for (int i = 0; i < 8; i++) {
                    receivedBits[i] = (byteRead >> (7 - i)) & 0x01;
                }
                int[] result = decodeByte(receivedBits);
                if (result[4] == 1) {
                    uncorrectedErrors++;
                }
                for (int i = 0; i < 4; i++) {
                    decodedByte += (result[i] << (bitsToMove - i));
                }
                if (iteration % 2 == 0) {
                    fos.write(decodedByte);
                    decodedByte = 0;
                    bitsToMove = 7;
                }
                else {
                    bitsToMove = 3;
                }
            }

            fis.close();
            fos.close();
            System.out.println("Uncorrected errors = " + uncorrectedErrors);
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the first/second file!");
        }
    }
}
