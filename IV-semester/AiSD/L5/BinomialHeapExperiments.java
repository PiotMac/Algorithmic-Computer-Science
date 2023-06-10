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
        printWriterInsertions.println("n;i;insertionComparisonsH1;insertionComparisonsH2");

        String fileNameExtractions = "BinomialHeapExtractions.csv";
        PrintWriter printWriterExtractions = new PrintWriter(new FileWriter(fileNameExtractions));
        printWriterExtractions.println("n;i;extractionComparisons");

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
                    H1.heapInsert(generatedNumber1);
                    insertionComparisonsH1.add(H1.operationComparisons);
                    H1.operationComparisons = 0;
                    int generatedNumber2 = randomHeapInsertion(generatedNumbers);
                    H2.heapInsert(generatedNumber2);
                    insertionComparisonsH2.add(H2.operationComparisons);
                    H2.operationComparisons = 0;
                }

                //Creating empty H heap
                BinomialHeap H = new BinomialHeap();

                //Heap-Union
                H.heapUnion(H1, H2);

                //Array of hopefully sorted extracted minimum values
                ArrayList<Integer> extractedValues = new ArrayList<>();
                ArrayList<Integer> extractionComparisons = new ArrayList<>();
                for (int x = 1; x <= 2 * n; x++) {
                    int extractedMinimumValue = H.extractMin();
                    extractedValues.add(extractedMinimumValue);
                    extractionComparisons.add(H.operationComparisons);
                    H.operationComparisons = 0;

                    if (isSorted(extractedValues)) {

                    }
                    else {
                        System.out.println("There is something wrong with extracting minimum values!\n");
                        throw new IllegalArgumentException();
                    }

                }

                if (H.isEmpty()) {
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
                }
            }
        }
        printWriterInsertions.close();
        printWriterExtractions.close();
    }

    //Function to generate unique integers for insertion
    private static int randomHeapInsertion(HashSet<Integer> generatedNumbers) {
        int sizeBeforeInsertion = generatedNumbers.size();
        int generatedNumber = random.nextInt(0, 10000);
        generatedNumbers.add(generatedNumber);
        int sizeAfterInsertion = generatedNumbers.size();
        while (sizeBeforeInsertion == sizeAfterInsertion) {
            generatedNumber = random.nextInt(0, 10000);
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
