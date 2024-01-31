import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;

public class Encoder {
    public static int encode4Bits(int[] dataBits) throws Exception {
        int[][] generatingMatrix = {
                {1, 1, 0, 1, 0, 0, 0},
                {0, 1, 1, 0, 1, 0, 0},
                {0, 0, 1, 1, 0, 1, 0},
                {0, 0, 0, 1, 1, 0, 1}
        };


        if (dataBits.length != 4) {
            throw new Exception("Four bits at a time have to be passed to the encoder!");
        }

        int[] encodedBits = new int[8];

        for (int i = 0; i < 7; i++) {
            encodedBits[i] = 0;
            for (int j = 0; j < 4; j++) {
                if (generatingMatrix[j][i] == 1) {
                    encodedBits[i] = (encodedBits[i] + dataBits[j]) % 2;
                }
            }
        }

        int parityCounter = 0;
        for (int i = 0; i < 7; i++) {
            if (encodedBits[i] == 1) {
                parityCounter ^= 1;
            }
        }
        encodedBits[7] = parityCounter;
        /*

        int parityBit1 = dataBits[0] ^ dataBits[1] ^ dataBits[3];
        int parityBit2 = dataBits[0] ^ dataBits[2] ^ dataBits[3];
        int parityBit3 = dataBits[1] ^ dataBits[2] ^ dataBits[3];
        int parityBit4 = dataBits[0] ^ dataBits[1] ^ dataBits[2] ^ dataBits[3] ^ parityBit1 ^ parityBit2 ^ parityBit3;

         */

        int result = 0;

        //String encodedDataString = "" + parityBit1 + parityBit2 + dataBits[0] + parityBit3 + dataBits[1]
        //        + dataBits[2] + dataBits[3] + parityBit4;

        //for (int i = 0; i < 8; i++) {
        //    if (encodedDataString.charAt(i) == '1') {
        //        result += (1 << (7 - i));
        //    }
        //}

        for (int i = 0; i < 8; i++) {
            if (encodedBits[i] == 1) {
                result += (1 << (7 - i));
            }
        }

        return result;
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Encoder <originalFile> <encodedFile>");
            return;
        }
        String originalFile = args[0];
        String encodedFile = args[1];

        try {
            FileInputStream fis = new FileInputStream(originalFile);
            FileOutputStream fos = new FileOutputStream(encodedFile);

            int byteRead;
            while ((byteRead = fis.read()) != -1) {
                int[] firstPart = new int[4];
                for (int i = 0; i < 4; i++) {
                    firstPart[i] = (byteRead >> (7 - i)) & 0x01;
                }
                fos.write(encode4Bits(firstPart));
                int[] secondPart = new int[4];
                for (int i = 0; i < 4; i++) {
                    secondPart[i] = (byteRead >> (3 - i)) & 0x01;
                }
                fos.write(encode4Bits(secondPart));
            }

            fis.close();
            fos.close();
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the first/second file!");
        }
    }
}
