import java.io.FileInputStream;
import java.io.FileOutputStream;

public class Decoder {
    private static int[] decodeByte(int[] dataBits) throws Exception {
        if (dataBits.length != 8) {
            throw new Exception("Eight bits at a time have to be passed to the decoder!");
        }

        int calculatedParityBit1 = dataBits[2] ^ dataBits[4] ^ dataBits[6];
        int calculatedParityBit2 = dataBits[2] ^ dataBits[5] ^ dataBits[6];
        int calculatedParityBit3 = dataBits[4] ^ dataBits[5] ^ dataBits[6];
        int calculatedParityBit4 = dataBits[2] ^ dataBits[4] ^ dataBits[5] ^ dataBits[6] ^ dataBits[0] ^ dataBits[1] ^ dataBits[3];

        int errorPosition = 0;
        int wasUncorrected = 0;

        if (dataBits[0] != calculatedParityBit1) {
            errorPosition += 1;
        }
        if (dataBits[1] != calculatedParityBit2) {
            errorPosition += 2;
        }
        if (dataBits[3] != calculatedParityBit3) {
            errorPosition += 4;
        }
        if (dataBits[7] != calculatedParityBit4) {
            if (errorPosition == 0) {
                dataBits[7] ^= 1;
            }
            else {
                dataBits[errorPosition - 1] ^= 1;
            }
        }
        else if (errorPosition != 0) {
            wasUncorrected = 1;
        }

        return new int[]{dataBits[2], dataBits[4], dataBits[5], dataBits[6], wasUncorrected};
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
