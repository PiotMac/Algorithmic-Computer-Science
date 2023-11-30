import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class OmegaCoding extends Coding {
    @Override
    public String encodeNumber(int number) {
        String output = "0";
        int k = number;
        String binaryRepresentation;
        while (k > 1) {
            binaryRepresentation = Integer.toBinaryString(k);
            output = binaryRepresentation + output;
            k = binaryRepresentation.length() - 1;
        }

        return output;
    }

    @Override
    public void encode(List<Integer> numbers, String outputFile) {
        try (FileOutputStream fos = new FileOutputStream(outputFile)) {

            int bitBuffer = 0;
            int bitCount = 0;

            for (int number : numbers) {
                String encodedNumber = encodeNumber(number);
                int encodedNumberLength = encodedNumber.length();
                for (int i = 0; i < encodedNumberLength; i++) {
                    char bit = encodedNumber.charAt(i);
                    bitBuffer <<= 1;
                    bitBuffer |= (bit == '1') ? 1 : 0;
                    bitCount++;

                    if (bitCount == 8) {
                        Coding.encodedFileBytes++;
                        fos.write(bitBuffer);
                        bitBuffer = 0;
                        bitCount = 0;
                    }
                }
            }
            // Write any remaining bits as 0s
            if (bitCount > 0) {
                Coding.encodedFileBytes++;
                bitBuffer <<= (8 - bitCount);
                fos.write(bitBuffer);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public List<Integer> decode(String inputFile) {
        List<Integer> decodedNumbers = new ArrayList<>();

        try (FileInputStream fis = new FileInputStream(inputFile)) {
            int byteRead;

            ArrayList<Integer> bits = new ArrayList<>();

            while((byteRead = fis.read()) != -1) {
                for (int i = 7; i >= 0; i--) {
                    int bit = (byteRead >> i) & 1;
                    bits.add(bit);
                }
            }

            int index = 0;

            while (index < bits.size()) {
                int n = 1;

                int currentBit = bits.get(index);
                StringBuilder stringBuilder;
                while (currentBit == 1) {

                    stringBuilder = new StringBuilder();
                    stringBuilder.append(1);

                    index++;
                    for (int i = 0; i < n; i++) {
                        if (index >= bits.size()) {
                            currentBit = 0;
                            break;
                        }
                        int nextBit = bits.get(index);
                        stringBuilder.append(nextBit);
                        index++;
                    }
                    n = Integer.parseInt(stringBuilder.toString(), 2);
                    if (index >= bits.size()) {
                        break;
                    }
                    else {
                        currentBit = bits.get(index);
                    }
                }
                // Check for padding
                if (index > bits.size() - 7 && n == 1) {
                    index++;
                }
                else {
                    decodedNumbers.add(n);
                    index++;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        decodedNumbers.replaceAll(integer -> integer - 1);

        return decodedNumbers;
    }
}
