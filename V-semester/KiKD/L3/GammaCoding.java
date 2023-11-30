import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class GammaCoding extends Coding {
    @Override
    public String encodeNumber(int number) {
        String binaryRepresentation = Integer.toBinaryString(number);
        int n = binaryRepresentation.length();

        return "0".repeat(n - 1) + binaryRepresentation;
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
            int zeros = 0;
            int totalZeros = 0;
            boolean inNumber = false;

            while ((bitBuffer = fis.read()) != -1) {
                for (int i = 7; i >= 0; i--) {
                    int bit = (bitBuffer >> i) & 1;
                    if (bit == 0 && !inNumber) {
                        zeros++;
                        currentNumber.append(bit);
                    }
                    else if(bit == 1 && !inNumber) {
                        if (zeros == 0) {
                            currentNumber.append(bit);
                            decodedNumbers.add(Integer.parseInt(currentNumber.toString(), 2));
                            currentNumber = new StringBuilder();
                        }
                        else {
                            inNumber = true;
                            totalZeros = zeros;
                            currentNumber.append(bit);
                            zeros--;
                        }
                    }
                    else if(zeros >= 0) {
                        currentNumber.append(bit);
                        zeros--;
                        if (zeros == -1) {
                            inNumber = false;
                            String currentNumberWithoutLeadingZeros = currentNumber.substring(totalZeros);
                            decodedNumbers.add(Integer.parseInt(currentNumberWithoutLeadingZeros, 2));
                            currentNumber = new StringBuilder();
                            zeros = 0;
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
