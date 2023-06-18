import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Random;

public class BinomialHeapExperiments {
    static Random random = new Random();

    public static void main(String[] args) throws IOException {
        String fileNameInsertions = "BinomialHeapInsertions.csv";
        PrintWriter printWriterInsertions = new PrintWriter(new FileWriter(fileNameInsertions));
        printWriterInsertions.println("n;i;iteration;insertionComparisonsH1;insertionComparisonsH2");

        String fileNameExtractions = "BinomialHeapExtractions.csv";
        PrintWriter printWriterExtractions = new PrintWriter(new FileWriter(fileNameExtractions));
        printWriterExtractions.println("n;i;iteration;extractionComparisons");

        String fileNameComparisons = "BinomialHeapComparisons.csv";
        PrintWriter printWriterComparisons = new PrintWriter(new FileWriter(fileNameComparisons));
        printWriterComparisons.println("n;comparisons");

        String fileNameConstComparisons = "BinomialHeapConstComparisons.csv";
        PrintWriter printWriterConstComparisons = new PrintWriter(new FileWriter(fileNameConstComparisons));
        printWriterConstComparisons.println("n;constComparisons");

        String fileNameAllComparisons = "BinomialHeapAllComparisons.csv";
        PrintWriter printWriterAllComparisons = new PrintWriter(new FileWriter(fileNameAllComparisons));
        printWriterAllComparisons.println("n;i;iteration;comparisons");

        for (int n = 500; n <= 1000; n+= 500) {
            for (int i = 1; i <= 5; i++) {
                //Creating empty H1 and H2 heaps
                BinomialHeap H1 = new BinomialHeap();
                BinomialHeap H2 = new BinomialHeap();

                //Making sure all the values are unique
                HashSet<Integer> generatedNumbers = new HashSet<>();
                ArrayList<Integer> insertionComparisonsH1 = new ArrayList<>();
                ArrayList<Integer> insertionComparisonsH2 = new ArrayList<>();
                for (int x = 1; x <= n; x++) {
                    int generatedNumber1 = randomHeapInsertion(generatedNumbers);
                    H1.insert(generatedNumber1);
                    insertionComparisonsH1.add(H1.operationComparisons);
                    H1.operationComparisons = 0;
                    int generatedNumber2 = randomHeapInsertion(generatedNumbers);
                    H2.insert(generatedNumber2);
                    insertionComparisonsH2.add(H2.operationComparisons);
                    H2.operationComparisons = 0;
                }

                for (int p = 1; p <= n; p++) {
                    printWriterAllComparisons.println(n + ";" + i + ";" + p + ";" + insertionComparisonsH1.get(p - 1));
                }
                for (int p = n + 1; p <= 2 * n; p++) {
                    printWriterAllComparisons.println(n + ";" + i + ";" + p + ";" + insertionComparisonsH2.get(p - 1 - n));
                }

                //Heap-Union
                H1.merge(H2);
                int iteration = 2 * n + 1;
                printWriterAllComparisons.println(n + ";" + i + ";" + iteration +";" + H1.operationComparisons);
                H1.operationComparisons = 0;

                //Array of hopefully sorted extracted minimum values
                ArrayList<Integer> extractedValues = new ArrayList<>();
                ArrayList<Integer> extractionComparisons = new ArrayList<>();
                for (int x = 1; x <= 2 * n; x++) {
                    int extractedMinimumValue = H1.extractMin();
                    extractedValues.add(extractedMinimumValue);
                    extractionComparisons.add(H1.operationComparisons);
                    H1.operationComparisons = 0;

                    if (isSorted(extractedValues)) {

                    }
                    else {
                        System.out.println("There is something wrong with extracting minimum values!\n");
                        throw new IllegalArgumentException();
                    }

                }

                if (H1.trees.isEmpty()) {
                    System.out.println("The H heap is empty!");
                }
                else {
                    System.out.println("Something went wrong!");
                }
                for (int p = 1; p <= n; p++) {
                    printWriterInsertions.println(n + ";" + i + ";" + p + ";" + insertionComparisonsH1.get(p - 1) + ";" + insertionComparisonsH2.get(p - 1));
                }
                for (int p = 1; p <= 2 * n; p++) {
                    printWriterExtractions.println(n + ";" + i + ";" + p + ";" + extractionComparisons.get(p - 1));
                    int it = p + 2 * n + 1;
                    printWriterAllComparisons.println(n + ";" + i + ";" + it + ";" + extractionComparisons.get(p - 1));
                }
            }
        }

        for (int n = 100; n <= 10000; n+= 100) {
            System.out.println("########## N = " + n + " ##########");
            double comparisonsPerN = 0.0;
            for (int i = 1; i <= 5; i++) {
                BinomialHeap H1 = new BinomialHeap();
                BinomialHeap H2 = new BinomialHeap();

                HashSet<Integer> generatedNumbers = new HashSet<>();

                for (int x = 1; x <= n; x++) {
                    int generatedNumber1 = randomHeapInsertion(generatedNumbers);
                    H1.insert(generatedNumber1);
                    comparisonsPerN += H1.operationComparisons;
                    H1.operationComparisons = 0;
                    int generatedNumber2 = randomHeapInsertion(generatedNumbers);
                    H2.insert(generatedNumber2);
                    comparisonsPerN += H2.operationComparisons;
                    H2.operationComparisons = 0;
                }

                H1.merge(H2);

                for (int x = 1; x <= 2 * n; x++) {
                    H1.extractMin();
                    comparisonsPerN += H1.operationComparisons;
                    H1.operationComparisons = 0;
                }
            }
            comparisonsPerN = comparisonsPerN / 5.0;
            printWriterComparisons.println(n + ";" + comparisonsPerN);
            comparisonsPerN = comparisonsPerN / n;
            printWriterConstComparisons.println(n + ";" + comparisonsPerN);
        }
        printWriterInsertions.close();
        printWriterExtractions.close();
        printWriterComparisons.close();
        printWriterConstComparisons.close();
    }

    //Function to generate unique integers for insertion
    private static int randomHeapInsertion(HashSet<Integer> generatedNumbers) {
        int sizeBeforeInsertion = generatedNumbers.size();
        int generatedNumber = random.nextInt(0, Integer.MAX_VALUE);
        generatedNumbers.add(generatedNumber);
        int sizeAfterInsertion = generatedNumbers.size();
        while (sizeBeforeInsertion == sizeAfterInsertion) {
            generatedNumber = random.nextInt(0, Integer.MAX_VALUE);
            generatedNumbers.add(generatedNumber);
            sizeAfterInsertion = generatedNumbers.size();
        }
        return generatedNumber;
    }

    public static boolean isSorted(ArrayList<Integer> list) {
        ArrayList<Integer> sortedList = new ArrayList<>(list);
        Collections.sort(sortedList);
        return list.equals(sortedList);
    }
}
