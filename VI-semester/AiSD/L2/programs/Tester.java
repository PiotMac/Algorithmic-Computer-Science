import java.util.Scanner;

public class Tester {
    public static void main(String[] args) {
        int mode = Integer.parseInt(args[0]);
        Scanner scanner = new Scanner(System.in);
        int size = scanner.nextInt();

        int[] originalKeys = new int[size];
        int[] sortedKeys = new int[size];

        for (int i = 0; i < size; i++) {
            originalKeys[i] = scanner.nextInt();//Integer.parseInt(args[i + 2]);
            sortedKeys[i] = originalKeys[i];
        }
        Utils utils = new Utils();
        Algorithms algorithm = new Algorithms();

        if (size < 40) {
            System.out.println("############### BEFORE ###############");
            utils.printArray(originalKeys);
            algorithm.setPrint(true);
        }


        switch (mode) {
            case 1:
                sortedKeys = algorithm.insertionSort(sortedKeys, size, utils);
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

        if (size < 40) {
            System.out.println("############### BEFORE ###############");
            utils.printArray(originalKeys);
            System.out.println("################ AFTER ###############");
            utils.printArray(sortedKeys);
        }

        System.out.println("#swaps       : " + utils.no_swaps);
        System.out.println("#comparisons : " + utils.no_comparisons);
        if (utils.isSorted(sortedKeys)) {
            System.out.println("SUCCESS!");
        }
        else {
            System.out.println("FAILURE!");
        }
    }
}
