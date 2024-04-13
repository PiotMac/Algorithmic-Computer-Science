import java.io.FileWriter;
import java.io.IOException;
import java.security.SecureRandom;

public class DataGenerator {
    static SecureRandom secureRandom = new SecureRandom();

    public static void main(String[] args) throws IOException {
        //FileWriter fileWriter = new FileWriter("data/smaller_data.csv");
        //FileWriter fileWriter = new FileWriter("data/bigger_data.csv");
        FileWriter fileWriter = new FileWriter("data/my_vs_merge.csv");

        fileWriter.write("type;n;k;comps;swaps" + "\n");
        int[] kValues = {1, 10, 100};
        int[] nValues = {10, 20, 30, 40, 50};

        int[] biggerNValues = new int[50];
        for (int i = 1; i <= 50; i++) {
            biggerNValues[i - 1] = 1000 * i;
        }



        Algorithms algorithm = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();
        Utils utils3 = new Utils();
        Utils utils4 = new Utils();
        Utils utils5 = new Utils();
        Utils utils6 = new Utils();
        for (int n : biggerNValues) {
        //for (int n : nValues) {
            for (int k : kValues) {

                //double averageComparisonsInsertion = 0.0;
                //double averageSwapsInsertion = 0.0;
                /*
                double averageComparisonsQuick = 0.0;
                double averageSwapsQuick = 0.0;
                double averageComparisonsHybrid = 0.0;
                double averageSwapsHybrid = 0.0;
                double averageComparisonsDual = 0.0;
                double averageSwapsDual = 0.0;

                 */


                double averageComparisonsRuns = 0.0;
                double averageSwapsRuns = 0.0;
                double averageComparisonsMerge = 0.0;
                double averageSwapsMerge = 0.0;
                for (int j = 0; j < k; j++) {
                    int[] originalKeys = new int[n];

                    int[] sortedKeys1 = new int[n];
                    int[] sortedKeys2 = new int[n];
                    int[] sortedKeys3 = new int[n];
                    int[] sortedKeys4 = new int[n];


                    int[] sortedKeys5 = new int[n];
                    int[] sortedKeys6 = new int[n];
                    for (int i = 0; i < n; i++) {
                        originalKeys[i] = secureRandom.nextInt(0, 2 * n);
                        sortedKeys1[i] = sortedKeys2[i] = sortedKeys3[i] = sortedKeys4[i] = sortedKeys5[i] = originalKeys[i];
                        //sortedKeys2[i] = sortedKeys3[i] = sortedKeys4[i] = originalKeys[i];
                        //sortedKeys6[i] = sortedKeys5[i] = originalKeys[i];
                    }

                    utils1.resetCounters();
                    utils2.resetCounters();
                    utils3.resetCounters();
                    utils4.resetCounters();

                    //algorithm.insertionSort(sortedKeys1, n, utils1);
                    //algorithm.quickSort(sortedKeys2, 0, n - 1, utils2);
                    //algorithm.hybridSort(sortedKeys3, 0, n - 1, utils3, 5);
                    //algorithm.dualSort(sortedKeys4, 0, n - 1, utils4);
                    //averageComparisonsInsertion += utils1.no_comparisons;
                    /*
                    averageComparisonsQuick += utils2.no_comparisons;
                    //averageSwapsInsertion += utils1.no_swaps;
                    averageSwapsQuick += utils2.no_swaps;
                    averageComparisonsHybrid += utils3.no_comparisons;
                    averageSwapsHybrid += utils3.no_swaps;
                    averageComparisonsDual += utils4.no_comparisons;
                    averageSwapsDual += utils4.no_swaps;

                     */


                    utils5.resetCounters();
                    utils6.resetCounters();

                    algorithm.improvisedSort(sortedKeys5, utils5);
                    algorithm.mergeSort(sortedKeys6, 0, sortedKeys6.length - 1, utils6);

                    averageComparisonsRuns += utils5.no_comparisons;
                    averageSwapsRuns += utils5.no_swaps;
                    averageComparisonsMerge += utils6.no_comparisons;
                    averageSwapsMerge += utils6.no_swaps;
                }

                //averageComparisonsInsertion /= k;
                //averageSwapsInsertion /= k;
                /*
                averageComparisonsQuick /= k;
                averageSwapsQuick /= k;
                averageComparisonsHybrid /= k;
                averageSwapsHybrid /= k;
                averageSwapsDual /= k;
                averageComparisonsDual /= k;

                 */
                //System.out.println("insertion;" + n + ";" + k + ";" + averageComparisonsInsertion + ";" + averageSwapsInsertion);
                /*
                System.out.println("quick;" + n + ";" + k + ";" + averageComparisonsQuick + ";" + averageSwapsQuick);
                System.out.println("hybrid;" + n + ";" + k + ";" + averageComparisonsHybrid + ";" + averageSwapsHybrid);
                System.out.println("dual;" + n + ";" + k + ";" + averageComparisonsDual + ";" + averageSwapsDual);
                //fileWriter.write("insertion;" + n + ";" + k + ";" + averageComparisonsInsertion + ";" + averageSwapsInsertion + "\n");
                fileWriter.write("quick;" + n + ";" + k + ";" + averageComparisonsQuick + ";" + averageSwapsQuick + "\n");
                fileWriter.write("hybrid;" + n + ";" + k + ";" + averageComparisonsHybrid + ";" + averageSwapsHybrid + "\n");
                fileWriter.write("dual;" + n + ";" + k + ";" + averageComparisonsDual + ";" + averageSwapsDual + "\n");

                 */

                averageComparisonsRuns /= k;
                averageSwapsRuns /= k;
                averageComparisonsMerge /= k;
                averageSwapsMerge /= k;

                System.out.println("merge;" + n + ";" + k + ";" + averageComparisonsMerge + ";" + averageSwapsMerge);
                System.out.println("my;" + n + ";" + k + ";" + averageComparisonsRuns + ";" + averageSwapsRuns);
                fileWriter.write("merge;" + n + ";" + k + ";" + averageComparisonsMerge + ";" + averageSwapsMerge + "\n");
                fileWriter.write("my;" + n + ";" + k + ";" + averageComparisonsRuns + ";" + averageSwapsRuns + "\n");
            }
        }
    }
}
