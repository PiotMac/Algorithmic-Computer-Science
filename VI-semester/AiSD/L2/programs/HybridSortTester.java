import java.io.FileWriter;
import java.io.IOException;
import java.security.SecureRandom;

public class HybridSortTester {
    public static final int MAX_THRESHOLD = 7;
    public static SecureRandom secureRandom = new SecureRandom();
    public static void main(String[] args) throws IOException {
        Algorithms algorithms = new Algorithms();
        Utils utils = new Utils();

        FileWriter fileWriter = new FileWriter("data/hybrid_testing.csv");
        fileWriter.write("size;threshold;comps;swaps\n");
        int[][] allKeys = new int[50][];
        for (int j = 1000; j <= 50000; j += 1000) {
            allKeys[j / 1000 - 1] = new int[j];
            for (int l = 0; l < j; l++) {
                allKeys[j / 1000 - 1][l] = secureRandom.nextInt(0, 2 * j);
            }
        }

        for (int i = 3; i <= MAX_THRESHOLD; i++) {
            for (int j = 1000; j <= 50000; j += 1000) {
                double averageComparisons = 0.0;
                double averageSwaps = 0.0;
                for (int k = 1; k <= 100; k++) {
                    utils.resetCounters();
                    int[] keys = new int[j];

                    System.arraycopy(allKeys[j / 1000 - 1], 0, keys, 0, j);

                    algorithms.hybridSort(keys, 0, j - 1, utils, i);
                    averageComparisons += utils.no_comparisons;
                    averageSwaps += utils.no_swaps;
                    if (!utils.isSorted(keys)) {
                        System.out.println("ERROR!");
                        return;
                    }
                }
                averageComparisons /= 100.0;
                averageSwaps /= 100.0;
                fileWriter.write(j + ";" + i + ";" + averageComparisons + ";" + averageSwaps + "\n");
                System.out.println(j + ";" + i + ";" + averageComparisons + ";" + averageSwaps);
            }
        }
    }
}
