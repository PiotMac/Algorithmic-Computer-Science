import java.io.FileWriter;
import java.io.IOException;
import java.security.SecureRandom;

public class DataGenerator {
    static SecureRandom random = new SecureRandom();

    public static void main(String[] args) throws IOException {
        generateDataForBST();
        generateDataForRBT();
        generateDataForSplay();
    }

    private static void generateDataForBST() throws IOException {
        FileWriter fileWriter = new FileWriter("data/bst.csv");
        fileWriter.write("type;n;comps;swaps;height" + "\n");

        int[] nValues = new int[10];
        for (int i = 1; i <= nValues.length; i++) {
            nValues[i - 1] = 10000 * i;
        }

        for (int n : nValues) {
            System.out.println("Testing for n = " + n);
            double averageComparisonsAvgCase = 0.0;
            double averageSwapsAvgCase = 0.0;
            double averageHeightAvgCase = 0.0;
            long maxComparisonsAvgCase = 0;
            long maxSwapsAvgCase = 0;
            long maxHeightAvgCase = 0;

            double averageComparisonsWorstCase = 0.0;
            double averageSwapsWorstCase = 0.0;
            double averageHeightWorstCase = 0.0;
            long maxComparisonsWorstCase = 0;
            long maxSwapsWorstCase = 0;
            long maxHeightWorstCase = 0;

            for (int k = 1; k <= 20; k++) {
                Utils utils1 = new Utils();
                Utils utils2 = new Utils();
                TreeBST treeAvgCase = new TreeBST(utils1);
                TreeBST treeWorstCase = new TreeBST(utils2);

                treeAvgCase.setPrintSize(n);
                treeWorstCase.setPrintSize(n);

                for (int i = 0; i < n; i++) {
                    treeAvgCase.insert(random.nextInt(0, 2 * n));
                    treeWorstCase.insert(2 * i);
                }

                for (int i = 0; i < n; i++) {
                    treeAvgCase.delete(random.nextInt(0, 2 * n));
                    treeWorstCase.delete(random.nextInt(0, 2 * n));
                }

                if (utils1.no_comparisons > maxComparisonsAvgCase) {
                    maxComparisonsAvgCase = utils1.no_comparisons;
                }
                if (utils2.no_comparisons > maxComparisonsWorstCase) {
                    maxComparisonsWorstCase = utils2.no_comparisons;
                }
                if (utils1.no_swaps > maxSwapsAvgCase) {
                    maxSwapsAvgCase = utils1.no_swaps;
                }
                if (utils2.no_swaps > maxSwapsWorstCase) {
                    maxSwapsWorstCase = utils2.no_swaps;
                }

                int avgTreeHeight = treeAvgCase.height();
                int worstTreeHeight = treeWorstCase.height();

                if (avgTreeHeight > maxHeightAvgCase) {
                    maxHeightAvgCase = avgTreeHeight;
                }
                if (worstTreeHeight > maxHeightWorstCase) {
                    maxHeightWorstCase = worstTreeHeight;
                }

                averageComparisonsAvgCase += utils1.no_comparisons;
                averageSwapsAvgCase += utils1.no_swaps;
                averageHeightAvgCase += avgTreeHeight;

                averageComparisonsWorstCase += utils2.no_comparisons;
                averageSwapsWorstCase += utils2.no_swaps;
                averageHeightWorstCase += worstTreeHeight;

                utils1.resetCounters();
                utils2.resetCounters();
            }

            averageComparisonsAvgCase /= 20;
            averageSwapsAvgCase /= 20;
            averageHeightAvgCase /= 20;
            averageComparisonsWorstCase /= 20;
            averageSwapsWorstCase /= 20;
            averageHeightWorstCase /= 20;

            System.out.print("avg;" + n + ";" + averageComparisonsAvgCase + ";" + averageSwapsAvgCase + ";" + averageHeightAvgCase + "\n");
            System.out.print("worst;" + n + ";" + averageComparisonsWorstCase + ";" + averageSwapsWorstCase + ";" + averageHeightWorstCase + "\n");
            System.out.print("max_avg;" + n + ";" + maxComparisonsAvgCase + ";" + maxSwapsAvgCase + ";" + maxHeightAvgCase + "\n");
            System.out.print("max_worst;" + n + ";" + maxComparisonsWorstCase + ";" + maxSwapsWorstCase + ";" + maxHeightWorstCase + "\n");

            fileWriter.write("avg;" + n + ";" + averageComparisonsAvgCase + ";" + averageSwapsAvgCase + ";" + averageHeightAvgCase + "\n");
            fileWriter.write("worst;" + n + ";" + averageComparisonsWorstCase + ";" + averageSwapsWorstCase + ";" + averageHeightWorstCase + "\n");
            fileWriter.write("max_avg;" + n + ";" + maxComparisonsAvgCase + ";" + maxSwapsAvgCase + ";" + maxHeightAvgCase + "\n");
            fileWriter.write("max_worst;" + n + ";" + maxComparisonsWorstCase + ";" + maxSwapsWorstCase + ";" + maxHeightWorstCase + "\n");
        }

        fileWriter.close();
    }

    private static void generateDataForRBT() throws IOException {
        FileWriter fileWriter = new FileWriter("data/rbt.csv");
        fileWriter.write("type;n;comps;swaps;height" + "\n");

        int[] nValues = new int[10];
        for (int i = 1; i <= nValues.length; i++) {
            nValues[i - 1] = 10000 * i;
        }

        for (int n : nValues) {
            System.out.println("Testing for n = " + n);
            double averageComparisonsAvgCase = 0.0;
            double averageSwapsAvgCase = 0.0;
            double averageHeightAvgCase = 0.0;
            long maxComparisonsAvgCase = 0;
            long maxSwapsAvgCase = 0;
            long maxHeightAvgCase = 0;

            double averageComparisonsWorstCase = 0.0;
            double averageSwapsWorstCase = 0.0;
            double averageHeightWorstCase = 0.0;
            long maxComparisonsWorstCase = 0;
            long maxSwapsWorstCase = 0;
            long maxHeightWorstCase = 0;

            for (int k = 1; k <= 20; k++) {
                Utils utils1 = new Utils();
                Utils utils2 = new Utils();
                TreeRBT treeAvgCase = new TreeRBT(utils1);
                TreeRBT treeWorstCase = new TreeRBT(utils2);

                treeAvgCase.setPrintSize(n);
                treeWorstCase.setPrintSize(n);

                for (int i = 0; i < n; i++) {
                    treeAvgCase.insert(random.nextInt(0, 2 * n));
                    treeWorstCase.insert(2 * i);
                }

                for (int i = 0; i < n; i++) {
                    treeAvgCase.delete(random.nextInt(0, 2 * n));
                    treeWorstCase.delete(random.nextInt(0, 2 * n));
                }

                if (utils1.no_comparisons > maxComparisonsAvgCase) {
                    maxComparisonsAvgCase = utils1.no_comparisons;
                }
                if (utils2.no_comparisons > maxComparisonsWorstCase) {
                    maxComparisonsWorstCase = utils2.no_comparisons;
                }
                if (utils1.no_swaps > maxSwapsAvgCase) {
                    maxSwapsAvgCase = utils1.no_swaps;
                }
                if (utils2.no_swaps > maxSwapsWorstCase) {
                    maxSwapsWorstCase = utils2.no_swaps;
                }

                int avgTreeHeight = treeAvgCase.height();
                int worstTreeHeight = treeWorstCase.height();

                if (avgTreeHeight > maxHeightAvgCase) {
                    maxHeightAvgCase = avgTreeHeight;
                }
                if (worstTreeHeight > maxHeightWorstCase) {
                    maxHeightWorstCase = worstTreeHeight;
                }

                averageComparisonsAvgCase += utils1.no_comparisons;
                averageSwapsAvgCase += utils1.no_swaps;
                averageHeightAvgCase += avgTreeHeight;

                averageComparisonsWorstCase += utils2.no_comparisons;
                averageSwapsWorstCase += utils2.no_swaps;
                averageHeightWorstCase += worstTreeHeight;

                utils1.resetCounters();
                utils2.resetCounters();
            }

            averageComparisonsAvgCase /= 20;
            averageSwapsAvgCase /= 20;
            averageHeightAvgCase /= 20;
            averageComparisonsWorstCase /= 20;
            averageSwapsWorstCase /= 20;
            averageHeightWorstCase /= 20;

            fileWriter.write("avg;" + n + ";" + averageComparisonsAvgCase + ";" + averageSwapsAvgCase + ";" + averageHeightAvgCase + "\n");
            fileWriter.write("worst;" + n + ";" + averageComparisonsWorstCase + ";" + averageSwapsWorstCase + ";" + averageHeightWorstCase + "\n");
            fileWriter.write("max_avg;" + n + ";" + maxComparisonsAvgCase + ";" + maxSwapsAvgCase + ";" + maxHeightAvgCase + "\n");
            fileWriter.write("max_worst;" + n + ";" + maxComparisonsWorstCase + ";" + maxSwapsWorstCase + ";" + maxHeightWorstCase + "\n");
        }

        fileWriter.close();
    }

    private static void generateDataForSplay() throws IOException {
        FileWriter fileWriter = new FileWriter("data/splay.csv");
        fileWriter.write("type;n;comps;swaps;height" + "\n");

        int[] nValues = new int[10];
        for (int i = 1; i <= nValues.length; i++) {
            nValues[i - 1] = 10000 * i;
        }

        for (int n : nValues) {
            System.out.println("Testing for n = " + n);
            double averageComparisonsAvgCase = 0.0;
            double averageSwapsAvgCase = 0.0;
            double averageHeightAvgCase = 0.0;
            long maxComparisonsAvgCase = 0;
            long maxSwapsAvgCase = 0;
            long maxHeightAvgCase = 0;

            double averageComparisonsWorstCase = 0.0;
            double averageSwapsWorstCase = 0.0;
            double averageHeightWorstCase = 0.0;
            long maxComparisonsWorstCase = 0;
            long maxSwapsWorstCase = 0;
            long maxHeightWorstCase = 0;

            for (int k = 1; k <= 20; k++) {
                Utils utils1 = new Utils();
                Utils utils2 = new Utils();
                TreeSplay treeAvgCase = new TreeSplay(utils1);
                TreeSplay treeWorstCase = new TreeSplay(utils2);

                treeAvgCase.setPrintSize(n);
                treeWorstCase.setPrintSize(n);

                for (int i = 0; i < n; i++) {
                    treeAvgCase.insert(random.nextInt(0, 2 * n));
                    treeWorstCase.insert(2 * i);
                }

                for (int i = 0; i < n; i++) {
                    treeAvgCase.delete(random.nextInt(0, 2 * n));
                    treeWorstCase.delete(random.nextInt(0, 2 * n));
                }

                if (utils1.no_comparisons > maxComparisonsAvgCase) {
                    maxComparisonsAvgCase = utils1.no_comparisons;
                }
                if (utils2.no_comparisons > maxComparisonsWorstCase) {
                    maxComparisonsWorstCase = utils2.no_comparisons;
                }
                if (utils1.no_swaps > maxSwapsAvgCase) {
                    maxSwapsAvgCase = utils1.no_swaps;
                }
                if (utils2.no_swaps > maxSwapsWorstCase) {
                    maxSwapsWorstCase = utils2.no_swaps;
                }

                int avgTreeHeight = treeAvgCase.height();
                int worstTreeHeight = treeWorstCase.height();

                if (avgTreeHeight > maxHeightAvgCase) {
                    maxHeightAvgCase = avgTreeHeight;
                }
                if (worstTreeHeight > maxHeightWorstCase) {
                    maxHeightWorstCase = worstTreeHeight;
                }

                averageComparisonsAvgCase += utils1.no_comparisons;
                averageSwapsAvgCase += utils1.no_swaps;
                averageHeightAvgCase += avgTreeHeight;

                averageComparisonsWorstCase += utils2.no_comparisons;
                averageSwapsWorstCase += utils2.no_swaps;
                averageHeightWorstCase += worstTreeHeight;

                utils1.resetCounters();
                utils2.resetCounters();
            }

            averageComparisonsAvgCase /= 20;
            averageSwapsAvgCase /= 20;
            averageHeightAvgCase /= 20;
            averageComparisonsWorstCase /= 20;
            averageSwapsWorstCase /= 20;
            averageHeightWorstCase /= 20;

            fileWriter.write("avg;" + n + ";" + averageComparisonsAvgCase + ";" + averageSwapsAvgCase + ";" + averageHeightAvgCase + "\n");
            fileWriter.write("worst;" + n + ";" + averageComparisonsWorstCase + ";" + averageSwapsWorstCase + ";" + averageHeightWorstCase + "\n");
            fileWriter.write("max_avg;" + n + ";" + maxComparisonsAvgCase + ";" + maxSwapsAvgCase + ";" + maxHeightAvgCase + "\n");
            fileWriter.write("max_worst;" + n + ";" + maxComparisonsWorstCase + ";" + maxSwapsWorstCase + ";" + maxHeightWorstCase + "\n");
        }

        fileWriter.close();
    }
}
