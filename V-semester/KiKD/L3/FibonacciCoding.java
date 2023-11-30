import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class FibonacciCoding extends Coding {
    public int fibonacciNumber(int number) {
        if (number < 2) return number;
        return fibonacciNumber(number - 1) + fibonacciNumber(number - 2);
    }

    @Override
    public String encodeNumber(int number) {
        int n = 2;
        int current = fibonacciNumber(n);
        int previous = current;

        while (current <= number) {
            n += 1;
            previous = current;
            current = fibonacciNumber(n);
        }
        int output = (int) Math.pow(2, (n - 1) - 2);
        number -= previous;

        while (number > 0) {
            n = 2;
            current = fibonacciNumber(n);
            previous = current;

            while (current <= number) {
                n++;
                previous = current;
                current = fibonacciNumber(n);
            }

            output += (int) Math.pow(2, (n - 1) - 2);
            number -= previous;
        }
        StringBuilder result = new StringBuilder();
        String r = Integer.toBinaryString(output).substring(1);
        result.append(r).reverse().append(1).append(1);

        return result.toString();
    }

    @Override
    public void encode(List<Integer> numbers, String outputFile) {
        try (FileOutputStream fos = new FileOutputStream(outputFile)) {

            int bitBuffer = 0;
            int bitCount = 0;
            //int j = 0;

            for (int number : numbers) {
                //System.out.println(100.0 * ((double) j) / (double) numbers.size());
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
                //j++;
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
            ArrayList<Integer> bits = new ArrayList<>();
            while ((bitBuffer = fis.read()) != -1) {
                for (int i = 7; i >= 0; i--) {
                    int bit = (bitBuffer >> i) & 1;
                    bits.add(bit);
                }
            }
            ArrayList<Integer> coefficients = new ArrayList<>();
            for (int i = 0; i < bits.size(); i++) {
                //System.out.println(100.0 * ((double) (i + 1) / (double) bits.size()));
                int bit = bits.get(i);
                int nextBit;
                if (i != bits.size() - 1) {
                    nextBit = bits.get(i + 1);
                    coefficients.add(bit);
                    if (bit == 1 && nextBit == 1) {
                        decodedNumbers.add(calculateCoefficients(coefficients));
                        coefficients.clear();
                        i++;
                    }
                }
                // Checking for padding
                else if (i == bits.size() - 1 && bits.get(i - 1) != 1) {
                    coefficients.clear();
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        decodedNumbers.replaceAll(integer -> integer - 1);

        return decodedNumbers;
    }

    private int calculateCoefficients(ArrayList<Integer> coefficients) {
        int result = 0;
        for (int i = 0; i < coefficients.size(); i++) {
            if (coefficients.get(i) == 1) {
                result += fibonacciNumber(i + 2);
            }
        }

        return result;
    }
}
