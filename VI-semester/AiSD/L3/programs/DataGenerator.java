import java.io.FileWriter;
import java.io.IOException;
import java.security.SecureRandom;

public class DataGenerator {
    static SecureRandom secureRandom = new SecureRandom();

    public static void main(String[] args) throws IOException {
        //testSelectVsRandomSelect();
        //testExperimentalSelect();
        //testBinarySearch();
        //testQuickSelect();
        testDualSelect();
    }

    private static void testSelectVsRandomSelect() throws IOException {
        FileWriter fileWriter = new FileWriter("data/select_vs_random.csv");

        fileWriter.write("type;n;comps;swaps" + "\n");

        // Values are as follows: {100, 200, ..., 50 000}
        int[] nValues = new int[500];
        for (int i = 1; i <= 500; i++) {
            nValues[i - 1] = 100 * i;
        }

        Algorithms algorithms = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();

        for (int n : nValues) {
            double averageComparisonsSelect = 0.0;
            double averageSwapsSelect = 0.0;
            double averageComparisonsRandomSelect = 0.0;
            double averageSwapsRandomSelect = 0.0;

            for (int m = 1; m <= 50; m++) {
                int[] originalKeys = new int[n];

                int[] sortedKeys1 = new int[n];
                int[] sortedKeys2 = new int[n];

                for (int i = 0; i < n; i++) {
                    originalKeys[i] = secureRandom.nextInt(0, 2 * n);
                    sortedKeys1[i] = sortedKeys2[i] = originalKeys[i];
                }

                int randomPosition = secureRandom.nextInt(1, n + 1);

                utils1.resetCounters();
                utils2.resetCounters();

                algorithms.select(sortedKeys1, 0, sortedKeys1.length - 1, randomPosition, utils1);
                algorithms.randomizedSelect(sortedKeys2, 0, sortedKeys2.length - 1, randomPosition, utils2);

                averageComparisonsSelect += utils1.no_comparisons;
                averageSwapsSelect += utils1.no_swaps;
                averageComparisonsRandomSelect += utils2.no_comparisons;
                averageSwapsRandomSelect += utils2.no_swaps;
            }

            averageComparisonsSelect /= 50;
            averageSwapsSelect /= 50;
            averageComparisonsRandomSelect /= 50;
            averageSwapsRandomSelect /= 50;

            System.out.println("select;" + n + ";" + averageComparisonsSelect + ";" + averageSwapsSelect);
            System.out.println("randomSelect;" + n + ";" + averageComparisonsRandomSelect + ";" + averageSwapsRandomSelect);
            fileWriter.write("select;" + n + ";" + averageComparisonsSelect + ";" + averageSwapsSelect + "\n");
            fileWriter.write("randomSelect;" + n + ";" + averageComparisonsRandomSelect + ";" + averageSwapsRandomSelect + "\n");
        }

        fileWriter.close();
    }

    private static void testExperimentalSelect() throws IOException {
        FileWriter fileWriter = new FileWriter("data/experimental_select.csv");

        fileWriter.write("type;n;comps;swaps;time" + "\n");

        // Values are as follows: {100, 200, ..., 50 000}
        int[] nValues = new int[500];
        for (int i = 1; i <= 500; i++) {
            nValues[i - 1] = 100 * i;
        }

        Algorithms algorithms = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();
        Utils utils3 = new Utils();
        Utils utils4 = new Utils();

        double start = 0.0;
        double finish = 0.0;
        double timeElapsedInMillis = 0.0;

        for (int n : nValues) {
            double averageComparisonsSelect3 = 0.0;
            double averageSwapsSelect3 = 0.0;
            double averageComparisonsSelect5 = 0.0;
            double averageSwapsSelect5 = 0.0;
            double averageComparisonsSelect7 = 0.0;
            double averageSwapsSelect7 = 0.0;
            double averageComparisonsSelect9 = 0.0;
            double averageSwapsSelect9 = 0.0;
            double averageTimeSelect3 = 0.0;
            double averageTimeSelect5 = 0.0;
            double averageTimeSelect7 = 0.0;
            double averageTimeSelect9 = 0.0;

            for (int m = 1; m <= 50; m++) {
                int[] originalKeys = new int[n];

                int[] sortedKeys1 = new int[n];
                int[] sortedKeys2 = new int[n];
                int[] sortedKeys3 = new int[n];
                int[] sortedKeys4 = new int[n];

                for (int i = 0; i < n; i++) {
                    originalKeys[i] = secureRandom.nextInt(0, 2 * n);
                    sortedKeys1[i] = sortedKeys2[i] = sortedKeys3[i] = sortedKeys4[i] = originalKeys[i];
                }

                int randomPosition = secureRandom.nextInt(1, n + 1);

                utils1.resetCounters();
                utils2.resetCounters();
                utils3.resetCounters();
                utils4.resetCounters();

                start = (double) System.nanoTime() / 1000000.0;
                algorithms.experimentalSelect(sortedKeys1, 0, sortedKeys1.length - 1, randomPosition, 3, utils1);
                finish = (double) System.nanoTime() / 1000000.0;
                timeElapsedInMillis = finish - start;
                averageTimeSelect3 += timeElapsedInMillis;

                start = (double) System.nanoTime() / 1000000.0;
                algorithms.experimentalSelect(sortedKeys2, 0, sortedKeys2.length - 1, randomPosition, 5, utils2);
                finish = (double) System.nanoTime() / 1000000.0;
                timeElapsedInMillis = finish - start;
                averageTimeSelect5 += timeElapsedInMillis;

                start = (double) System.nanoTime() / 1000000.0;
                algorithms.experimentalSelect(sortedKeys3, 0, sortedKeys3.length - 1, randomPosition, 7, utils3);
                finish = (double) System.nanoTime() / 1000000.0;
                timeElapsedInMillis = finish - start;
                averageTimeSelect7 += timeElapsedInMillis;

                start = (double) System.nanoTime() / 1000000.0;
                algorithms.experimentalSelect(sortedKeys4, 0, sortedKeys4.length - 1, randomPosition, 9, utils4);
                finish = (double) System.nanoTime() / 1000000.0;
                timeElapsedInMillis = finish - start;
                averageTimeSelect9 += timeElapsedInMillis;


                averageComparisonsSelect3 += utils1.no_comparisons;
                averageSwapsSelect3 += utils1.no_swaps;
                averageComparisonsSelect5 += utils2.no_comparisons;
                averageSwapsSelect5 += utils2.no_swaps;
                averageComparisonsSelect7 += utils3.no_comparisons;
                averageSwapsSelect7 += utils3.no_swaps;
                averageComparisonsSelect9 += utils4.no_comparisons;
                averageSwapsSelect9 += utils4.no_swaps;
            }

            averageComparisonsSelect3 /= 50;
            averageSwapsSelect3 /= 50;
            averageComparisonsSelect5 /= 50;
            averageSwapsSelect5 /= 50;
            averageComparisonsSelect7 /= 50;
            averageSwapsSelect7 /= 50;
            averageComparisonsSelect9 /= 50;
            averageSwapsSelect9 /= 50;
            averageTimeSelect3 /= 50;
            averageTimeSelect5 /= 50;
            averageTimeSelect7 /= 50;
            averageTimeSelect9 /= 50;

            fileWriter.write("select3;" + n + ";" + averageComparisonsSelect3 + ";" + averageSwapsSelect3 + ";" + averageTimeSelect3 + "\n");
            fileWriter.write("select5;" + n + ";" + averageComparisonsSelect5 + ";" + averageSwapsSelect5 + ";" + averageTimeSelect5 + "\n");
            fileWriter.write("select7;" + n + ";" + averageComparisonsSelect7 + ";" + averageSwapsSelect7 + ";" + averageTimeSelect7 + "\n");
            fileWriter.write("select9;" + n + ";" + averageComparisonsSelect9 + ";" + averageSwapsSelect9 + ";" + averageTimeSelect9 + "\n");
        }

        fileWriter.close();
    }

    private static void testBinarySearch() throws IOException {
        FileWriter fileWriter = new FileWriter("data/binary_search.csv");

        fileWriter.write("type;n;comps;time" + "\n");

        // Values are as follows: {1000, 2000, ..., 100 000}
        int[] nValues = new int[100];
        for (int i = 1; i <= 100; i++) {
            nValues[i - 1] = 1000 * i;
        }

        Algorithms algorithms = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();
        Utils utils3 = new Utils();
        Utils utils4 = new Utils();
        Utils utils5 = new Utils();

        double start = 0.0;
        double finish = 0.0;
        double timeElapsedInNanoSeconds = 0.0;

        for (int n : nValues) {
            double averageComparisonsBeginning = 0.0;
            double averageTimeBeginning = 0.0;
            double averageComparisonsMiddle = 0.0;
            double averageTimeMiddle = 0.0;
            double averageComparisonsEnd = 0.0;
            double averageTimeEnd = 0.0;
            double averageComparisonsMissing = 0.0;
            double averageTimeMissing = 0.0;
            double averageComparisonsRandom = 0.0;
            double averageTimeRandom = 0.0;

            for (int m = 1; m <= 50; m++) {
                int[] originalKeys = new int[n];

                int[] sortedKeys1 = new int[n];
                int[] sortedKeys2 = new int[n];
                int[] sortedKeys3 = new int[n];
                int[] sortedKeys4 = new int[n];
                int[] sortedKeys5 = new int[n];


                for (int i = 0; i < n; i++) {
                    originalKeys[i] = i;
                    sortedKeys1[i] = sortedKeys2[i] = sortedKeys3[i] = sortedKeys4[i] = sortedKeys5[i] = originalKeys[i];
                }

                int randomValueBeginning = secureRandom.nextInt(0, n / 10);
                int randomValueMiddle = secureRandom.nextInt(4 * n / 10, 6 * n / 10);
                int randomValueEnd = secureRandom.nextInt(9 * n / 10, n);
                int randomValueMissing = secureRandom.nextInt(n, 2 * n);
                int randomValueRandom = secureRandom.nextInt(0, n);

                utils1.resetCounters();
                utils2.resetCounters();
                utils3.resetCounters();
                utils4.resetCounters();
                utils5.resetCounters();

                start = System.nanoTime();
                algorithms.binarySearch(sortedKeys1, 0, sortedKeys1.length - 1, randomValueBeginning, utils1);
                finish = System.nanoTime();
                timeElapsedInNanoSeconds  = finish - start;
                averageTimeBeginning += timeElapsedInNanoSeconds;

                start = System.nanoTime();
                algorithms.binarySearch(sortedKeys2, 0, sortedKeys2.length - 1, randomValueMiddle, utils2);
                finish = System.nanoTime();
                timeElapsedInNanoSeconds  = finish - start;
                averageTimeMiddle += timeElapsedInNanoSeconds;

                start = System.nanoTime();
                algorithms.binarySearch(sortedKeys3, 0, sortedKeys3.length - 1, randomValueEnd, utils3);
                finish = System.nanoTime();
                timeElapsedInNanoSeconds  = finish - start;
                averageTimeEnd += timeElapsedInNanoSeconds;

                start = System.nanoTime();
                algorithms.binarySearch(sortedKeys4, 0, sortedKeys4.length - 1, randomValueMissing, utils4);
                finish = System.nanoTime();
                timeElapsedInNanoSeconds  = finish - start;
                averageTimeMissing += timeElapsedInNanoSeconds;

                start = System.nanoTime();
                algorithms.binarySearch(sortedKeys5, 0, sortedKeys5.length - 1, randomValueRandom, utils5);
                finish = System.nanoTime();
                timeElapsedInNanoSeconds  = finish - start;
                averageTimeRandom += timeElapsedInNanoSeconds;

                averageComparisonsBeginning += utils1.no_comparisons;
                averageComparisonsMiddle += utils2.no_comparisons;
                averageComparisonsEnd += utils3.no_comparisons;
                averageComparisonsMissing += utils4.no_comparisons;
                averageComparisonsRandom += utils5.no_comparisons;
            }

            averageComparisonsBeginning /= 50;
            averageComparisonsMiddle /= 50;
            averageComparisonsEnd /= 50;
            averageComparisonsMissing /= 50;
            averageComparisonsRandom /= 50;

            averageTimeBeginning /= 50;
            averageTimeMiddle /= 50;
            averageTimeEnd /= 50;
            averageTimeMissing /= 50;
            averageTimeRandom /= 50;

            fileWriter.write("beginning;" + n + ";" + averageComparisonsBeginning + ";" + averageTimeBeginning + "\n");
            fileWriter.write("middle;" + n + ";" + averageComparisonsMiddle + ";" + averageTimeMiddle + "\n");
            fileWriter.write("end;" + n + ";" + averageComparisonsEnd + ";" + averageTimeEnd + "\n");
            fileWriter.write("missing;" + n + ";" + averageComparisonsMissing + ";" + averageTimeMissing + "\n");
            fileWriter.write("random;" + n + ";" + averageComparisonsRandom + ";" + averageTimeRandom + "\n");
        }

        fileWriter.close();
    }

    private static void testQuickSelect() throws IOException {
        //FileWriter fileWriter = new FileWriter("data/quickselect_vs_quicksort_worst.csv");
        FileWriter fileWriter = new FileWriter("data/quickselect_vs_quicksort_average.csv");

        fileWriter.write("type;n;comps;swaps;time" + "\n");

        // Values are as follows: {100, 200, ..., 10 000}
        int[] nValues = new int[100];
        for (int i = 1; i <= 100; i++) {
            nValues[i - 1] = 100 * i;
        }

        Algorithms algorithms = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();

        double start = 0.0;
        double finish = 0.0;
        double timeElapsedInMilliSeconds = 0.0;

        for (int n : nValues) {
            double averageComparisonsQuickSelect = 0.0;
            double averageSwapsQuickSelect = 0.0;
            double averageTimeQuickSelect = 0.0;
            double averageComparisonsQuickSort = 0.0;
            double averageSwapsQuickSort = 0.0;
            double averageTimeQuickSort = 0.0;

            for (int m = 1; m <= 50; m++) {
                int[] originalKeys = new int[n];

                int[] sortedKeys1 = new int[n];
                int[] sortedKeys2 = new int[n];

                for (int i = 0; i < n; i++) {
                    // WORST CASE:
                    //originalKeys[i] = 2 * n - 1 - i;
                    // AVERAGE CASE:
                    originalKeys[i] = secureRandom.nextInt(0, 2 * n);
                    sortedKeys1[i] = sortedKeys2[i] = originalKeys[i];
                }

                utils1.resetCounters();
                utils2.resetCounters();

                start = System.nanoTime();
                algorithms.quickSortSelect(sortedKeys1, 0, sortedKeys1.length - 1, utils1);
                finish = System.nanoTime();
                timeElapsedInMilliSeconds  = (finish - start) / 1000000.0;
                averageTimeQuickSelect += timeElapsedInMilliSeconds;

                start = System.nanoTime();
                algorithms.quickSort(sortedKeys2, 0, sortedKeys2.length - 1, utils2);
                finish = System.nanoTime();
                timeElapsedInMilliSeconds = (finish - start) / 1000000.0;
                averageTimeQuickSort += timeElapsedInMilliSeconds;

                averageComparisonsQuickSelect += utils1.no_comparisons;
                averageComparisonsQuickSort += utils2.no_comparisons;
                averageSwapsQuickSelect += utils1.no_swaps;
                averageSwapsQuickSort += utils2.no_swaps;
            }

            averageComparisonsQuickSelect /= 50;
            averageComparisonsQuickSort /= 50;
            averageSwapsQuickSelect /= 50;
            averageSwapsQuickSort /= 50;
            averageTimeQuickSelect /= 50;
            averageTimeQuickSort /= 50;

            fileWriter.write("quickselect;" + n + ";" + averageComparisonsQuickSelect + ";" + averageSwapsQuickSelect + ";" + averageTimeQuickSelect + "\n");
            fileWriter.write("quicksort;" + n + ";" + averageComparisonsQuickSort + ";" + averageSwapsQuickSort + ";" + averageTimeQuickSort + "\n");
        }

        fileWriter.close();
    }

    private static void testDualSelect() throws IOException {
        FileWriter fileWriter = new FileWriter("data/dualselect_vs_dualsort_worst.csv");
        //FileWriter fileWriter = new FileWriter("data/dualselect_vs_dualsort_average.csv");

        fileWriter.write("type;n;comps;swaps;time" + "\n");

        // Values are as follows: {100, 200, ..., 10 000}
        int[] nValues = new int[100];
        for (int i = 1; i <= 100; i++) {
            nValues[i - 1] = 100 * i;
        }

        Algorithms algorithms = new Algorithms();
        Utils utils1 = new Utils();
        Utils utils2 = new Utils();

        double start = 0.0;
        double finish = 0.0;
        double timeElapsedInMilliSeconds = 0.0;

        for (int n : nValues) {
            double averageComparisonsDualSelect = 0.0;
            double averageSwapsDualSelect = 0.0;
            double averageTimeDualSelect = 0.0;
            double averageComparisonsDualSort = 0.0;
            double averageSwapsDualSort = 0.0;
            double averageTimeDualSort = 0.0;

            for (int m = 1; m <= 50; m++) {
                int[] originalKeys = new int[n];

                int[] sortedKeys1 = new int[n];
                int[] sortedKeys2 = new int[n];

                for (int i = 0; i < n; i++) {
                    // WORST CASE:
                    originalKeys[i] = 2 * n - 1 - i;
                    // AVERAGE CASE:
                    //originalKeys[i] = secureRandom.nextInt(0, 2 * n);
                    sortedKeys1[i] = sortedKeys2[i] = originalKeys[i];
                }

                utils1.resetCounters();
                utils2.resetCounters();

                start = System.nanoTime();
                algorithms.dualSortSelect(sortedKeys1, 0, sortedKeys1.length - 1, utils1);
                finish = System.nanoTime();
                timeElapsedInMilliSeconds  = (finish - start) / 1000000.0;
                averageTimeDualSelect += timeElapsedInMilliSeconds;

                start = System.nanoTime();
                algorithms.dualSort(sortedKeys2, 0, sortedKeys2.length - 1, utils2);
                finish = System.nanoTime();
                timeElapsedInMilliSeconds = (finish - start) / 1000000.0;
                averageTimeDualSort += timeElapsedInMilliSeconds;

                averageComparisonsDualSelect += utils1.no_comparisons;
                averageComparisonsDualSort += utils2.no_comparisons;
                averageSwapsDualSelect += utils1.no_swaps;
                averageSwapsDualSort += utils2.no_swaps;
            }

            averageComparisonsDualSelect /= 50;
            averageComparisonsDualSort /= 50;
            averageSwapsDualSelect /= 50;
            averageSwapsDualSort /= 50;
            averageTimeDualSelect /= 50;
            averageTimeDualSort /= 50;

            fileWriter.write("dualselect;" + n + ";" + averageComparisonsDualSelect + ";" + averageSwapsDualSelect + ";" + averageTimeDualSelect + "\n");
            fileWriter.write("dualsort;" + n + ";" + averageComparisonsDualSort + ";" + averageSwapsDualSort + ";" + averageTimeDualSort + "\n");
        }

        fileWriter.close();
    }
}
