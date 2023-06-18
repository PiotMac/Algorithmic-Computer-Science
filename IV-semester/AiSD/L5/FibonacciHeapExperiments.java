import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Random;

public class FibonacciHeapExperiments {
    static Random random = new Random();

    public static void main(String[] args) throws IOException {
        String fileNameInsertions = "FibonacciHeapInsertions.csv";
        PrintWriter printWriterInsertions = new PrintWriter(new FileWriter(fileNameInsertions));
        printWriterInsertions.println("n;i;insertionComparisonsH1;insertionComparisonsH2");

        String fileNameExtractions = "FibonacciHeapExtractions.csv";
        PrintWriter printWriterExtractions = new PrintWriter(new FileWriter(fileNameExtractions));
        printWriterExtractions.println("n;i;extractionComparisons");

        String fileNameComparisons = "FibonacciHeapComparisons.csv";
        PrintWriter printWriterComparisons = new PrintWriter(new FileWriter(fileNameComparisons));
        printWriterComparisons.println("n;comparisons");

        String fileNameConstComparisons = "FibonacciHeapConstComparisons.csv";
        PrintWriter printWriterConstComparisons = new PrintWriter(new FileWriter(fileNameConstComparisons));
        printWriterConstComparisons.println("n;comparisons");

        String fileNameAllComparisons = "FibonacciHeapAllComparisons.csv";
        PrintWriter printWriterAllComparisons = new PrintWriter(new FileWriter(fileNameAllComparisons));
        printWriterAllComparisons.println("n;i;comparisons");

        for (int n = 500; n <= 1000; n+= 500) {
            for (int i = 1; i <= 5; i++) {
                //Creating empty H1 and H2 heaps
                FibonacciHeap H1 = new FibonacciHeap();
                FibonacciHeap H2 = new FibonacciHeap();

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

                for (int p = 0; p < n; p++) {
                    printWriterAllComparisons.println(n + ";" + i + ";" + insertionComparisonsH1.get(p));
                }
                for (int p = 0; p < n; p++) {
                    printWriterAllComparisons.println(n + ";" + i + ";" + insertionComparisonsH2.get(p));
                }

                //Heap-Union
                H1.merge(H2);
                printWriterAllComparisons.println(n + ";" + i + ";" + H1.operationComparisons);
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
                for (int p = 0; p < n; p++) {
                    printWriterInsertions.println(n + ";" + i + ";" + insertionComparisonsH1.get(p) + ";" + insertionComparisonsH2.get(p));
                }
                for (int p = 0; p < 2 * n; p++) {
                    printWriterExtractions.println(n + ";" + i + ";" + extractionComparisons.get(p));
                    printWriterAllComparisons.println(n + ";" + i + ";" + extractionComparisons.get(p));
                }
            }
        }

        for (int n = 100; n <= 10000; n+= 100) {
            System.out.println("########## N = " + n + " ##########");
            double comparisonsPerN = 0.0;
            for (int i = 1; i <= 5; i++) {
                FibonacciHeap H1 = new FibonacciHeap();
                FibonacciHeap H2 = new FibonacciHeap();

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
        printWriterAllComparisons.close();
    }

    //Function to generate unique integers for insertion
    private static int randomHeapInsertion(HashSet<Integer> generatedNumbers) {
        int sizeBeforeInsertion = generatedNumbers.size();
        int generatedNumber = random.nextInt(0, 100000);
        generatedNumbers.add(generatedNumber);
        int sizeAfterInsertion = generatedNumbers.size();
        while (sizeBeforeInsertion == sizeAfterInsertion) {
            generatedNumber = random.nextInt(0, 100000);
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
