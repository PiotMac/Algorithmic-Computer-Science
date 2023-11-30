import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LZW {
    public static int totalBytes = 0;
    public List<Integer> encode(String inputFileName) {
        List<Integer> compressedData = new ArrayList<>();
        try (FileInputStream fis = new FileInputStream(inputFileName)){

            Map<String, Integer> dictionary = new HashMap<>();
            int nextCode = 256;

            // Inicjalizacja słownika
            for (int i = 0; i < 256; i++) {
                dictionary.put("" + (char) i, i);
            }

            int byteRead;
            int it = 0;
            String currentInput = "";
            while ((byteRead = fis.read()) != -1) {
                totalBytes++;
                if (it == 0) {
                    currentInput = "" + (char) (byteRead);
                    it++;
                }
                else {
                    char nextChar = (char) (byteRead);
                    String extended = currentInput + nextChar;
                    if (dictionary.containsKey(extended)) {
                        currentInput = extended;
                    } else {
                        dictionary.put(extended, nextCode);
                        compressedData.add(dictionary.get(currentInput));
                        nextCode++;
                        currentInput = "" + nextChar;
                    }
                }
            }
            compressedData.add(dictionary.get(currentInput));


        } catch (IOException e) {
            e.printStackTrace();
        }
        compressedData.replaceAll(integer -> integer + 1);

        return compressedData;
    }

    public void decode(List<Integer> encodedInformation, String decompressedFileName) {
        try (FileOutputStream fos = new FileOutputStream(decompressedFileName)){

            Map<Integer, String> dictionary = new HashMap<>();
            ArrayList<Byte> out = new ArrayList<>();

            for (int i = 0; i < 256; i++) {
                String ch = "";
                ch += (char) i;
                dictionary.put(i, ch);
            }

            // Next codeword to be inserted into the dictionary will get this index
            int newCodewordIndex = 256;
            // First number
            int prev = encodedInformation.get(0); // OLD
            String s = dictionary.get(prev); // S
            String c = ""; // C
            c += s.charAt(0);
            out.add((byte) c.charAt(0));

            for (int i = 1; i < encodedInformation.size(); i++) {
                int curr = encodedInformation.get(i); // NEW

                if (!dictionary.containsKey(curr)) {
                    s = dictionary.get(prev);
                    s += c;
                } else {
                    s = dictionary.get(curr);
                }

                for (int j = 0; j < s.length(); j++) {
                    out.add((byte) s.charAt(j));
                }
                c = "";
                c += s.charAt(0);
                dictionary.put(newCodewordIndex, dictionary.get(prev) + c);
                newCodewordIndex++;
                prev = curr;
            }

            for (Byte aByte : out) {
                fos.write(aByte);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
