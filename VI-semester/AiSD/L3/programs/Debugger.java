import java.security.SecureRandom;

public class Debugger {
    public static SecureRandom secureRandom = new SecureRandom();
    public static final int SIZE = 300000;
    public static final int POSITION = SIZE / 2 + SIZE / 4;
    public static final int MODE = 6;

    public static void main(String[] args) {

        int[] originalKeys = new int[SIZE];
        int[] sortedKeys = new int[SIZE];

        for (int i = 0; i < SIZE; i++) {
            //2 * SIZE - 1 - i;
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

        int foundValue = 0;

        switch (MODE) {
            case 1:
                foundValue = algorithm.select(sortedKeys, 0, SIZE - 1, POSITION, utils);
                break;
            case 2:
                foundValue = algorithm.randomizedSelect(originalKeys, 0, originalKeys.length - 1, 13, utils);
                break;
            case 3:
                foundValue = algorithm.experimentalSelect(originalKeys, 0, originalKeys.length - 1, 13, 3, utils);
                break;
            case 5:
                sortedKeys = algorithm.quickSortSelect(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
            case 6:
                sortedKeys = algorithm.dualSortSelect(sortedKeys, 0, sortedKeys.length - 1, utils);
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

        //algorithm.insertionSort(sortedKeys, 0, SIZE - 1, utils);

        if (SIZE < 40) {
            System.out.println("################ AFTER ###############");
            utils.printArray(sortedKeys);
        }

        if (utils.isSorted(sortedKeys)) {
            System.out.println("SUCCESS!!!");
        }

        System.out.println(foundValue + " is the same as " + sortedKeys[POSITION - 1]);
        if (foundValue == sortedKeys[POSITION - 1]) {
            System.out.println(foundValue + " is the same as " + sortedKeys[POSITION - 1]);
            System.out.println("SUCCESS!");
        }
    }
}
