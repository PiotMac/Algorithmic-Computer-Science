import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class LCSDataGenerator {
    public static void main(String[] args) {
        String fileName = "LCS_data.csv";
        LongestCommonSubsequence longestCommonSubsequence = new LongestCommonSubsequence();
        try {
            PrintWriter printWriter = new PrintWriter(new FileWriter(fileName));
            printWriter.println("n;i;LCS;time");
            for (int n = 1000; n <= 5000; n += 100) {
                double averageTime = 0.0;
                double averageLCS = 0.0;
                System.out.println("########## TEST FOR N = " + n + " ##########");
                for (int i = 1; i <= 100; i++) {
                    String X = longestCommonSubsequence.randomStringGenerator(n);
                    String Y = longestCommonSubsequence.randomStringGenerator(n);
                    long startTime = System.nanoTime();
                    int result = longestCommonSubsequence.lcsAlgorithm(X, Y, n, n);
                    long endTime = System.nanoTime();
                    long resultTime = endTime - startTime;
                    averageTime += resultTime / 1000000.0;
                    averageLCS += result;
                    printWriter.println(n + ";" + i + ";" + result + ";" + resultTime / 1000000.0);
                }
                printWriter.println(n + ";-1;" + averageLCS / 100.0 + ";" + averageTime / 100.0);
                printWriter.println(n + ";-1;" + averageLCS / (100.0 * n) + ";" + averageTime / (100.0 * Math.pow(n, 2)));
            }
            printWriter.close();
            System.out.println("Data has been written to the file: " + fileName);
        }
        catch (IOException e) {
            System.out.println("An error occurred while writing to the file: " + e.getMessage());
        }
    }
}
