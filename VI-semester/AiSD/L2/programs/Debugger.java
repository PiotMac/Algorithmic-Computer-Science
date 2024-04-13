import java.security.SecureRandom;

public class Debugger {
    public static SecureRandom secureRandom = new SecureRandom();
    public static final int SIZE = 8;
    public static final int MODE = 5;

    public static void main(String[] args) {

        int[] originalKeys = new int[SIZE];
        int[] sortedKeys = new int[SIZE];

        for (int i = 0; i < SIZE; i++) {
            originalKeys[i] = secureRandom.nextInt(0, 2 * SIZE);
            sortedKeys[i] = originalKeys[i];
        }
        Utils utils = new Utils();
        Algorithms algorithm = new Algorithms();

        if (SIZE < 40) {
            System.out.println("############### BEFORE ###############");
            utils.printArray(originalKeys);
            algorithm.setPrint(true);
        }


        switch (MODE) {
            case 1:
                sortedKeys = algorithm.insertionSort(sortedKeys, SIZE, utils);
                break;
            case 2:
                sortedKeys = algorithm.quickSort(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
            case 3:
                sortedKeys = algorithm.hybridSort(sortedKeys, 0, sortedKeys.length - 1, utils, 5);
                break;
            case 4:
                sortedKeys = algorithm.dualSort(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
            case 5:
                sortedKeys = algorithm.improvisedSort(sortedKeys, utils);
                break;
            case 6:
                sortedKeys = algorithm.mergeSort(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
        }

        if (SIZE < 40) {
            System.out.println("############### BEFORE ###############");
            utils.printArray(originalKeys);
            System.out.println("################ AFTER ###############");
            utils.printArray(sortedKeys);
        }

        System.out.println("#swaps       : " + utils.no_swaps);
        System.out.println("#comparisons : " + utils.no_comparisons);
        if (utils.isSorted(sortedKeys)) {
            System.out.println("SUCCESS!");
        } else {
            System.out.println("FAILURE!");
        }
    }
}
