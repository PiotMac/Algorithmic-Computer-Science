import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class DeltaCoding extends Coding {
    @Override
    public String encodeNumber(int number) {
        String binaryRepresentation = Integer.toBinaryString(number);
        int n = binaryRepresentation.length();

        String nBinary = Integer.toBinaryString(n);
        int k = nBinary.length();

        return "0".repeat(k - 1) + nBinary + binaryRepresentation.substring(1);
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

            int bitBuffer;
            StringBuilder currentNumber = new StringBuilder();
            StringBuilder currentNumberLength = new StringBuilder();
            int zeros = 0;
            boolean inSize = false;
            boolean inNumber = false;

            int n = 0;

            while ((bitBuffer = fis.read()) != -1) {

                for (int i = 7; i >= 0; i--) {
                    int bit = (bitBuffer >> i) & 1;
                    if (bit == 0 && !inSize && !inNumber) {
                        zeros++;
                    }
                    else if(bit == 1 && !inSize && !inNumber) {
                        currentNumber.append(bit);
                        currentNumberLength.append(bit);
                        if (zeros == 0) {
                            decodedNumbers.add(Integer.parseInt(currentNumber.toString(), 2));
                            currentNumber = new StringBuilder();
                            currentNumberLength = new StringBuilder();
                        }
                        else {
                            inSize = true;
                        }
                    }
                    else if(zeros > 0) {
                        zeros--;
                        currentNumberLength.append(bit);
                        if (zeros == 0) {
                            n = Integer.parseInt(currentNumberLength.toString(), 2);
                            inNumber = true;
                            inSize = false;
                        }
                    }
                    else if(!inSize && n != 0) {
                        n--;
                        if (n == 1) {
                            inNumber = false;
                            currentNumber.append(bit);
                            decodedNumbers.add(Integer.parseInt(currentNumber.toString(), 2));
                            currentNumber = new StringBuilder();
                            currentNumberLength = new StringBuilder();
                        }
                        else {
                            currentNumber.append(bit);
                        }
                    }
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        decodedNumbers.replaceAll(integer -> integer - 1);

        return decodedNumbers;
    }
}
