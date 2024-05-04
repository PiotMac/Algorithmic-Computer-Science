import java.util.Scanner;

public class Tester {
    public static void main(String[] args) {
        int mode = Integer.parseInt(args[0]);
        Scanner scanner = new Scanner(System.in);
        int size = scanner.nextInt();
        int position = scanner.nextInt();

        if ((mode == 1 || mode == 2 || mode == 3) && (position > size || position < 1)) {
            System.out.println("Position is out of bounds!");
            return;
        }

        int[] originalKeys = new int[size];
        int[] sortedKeys = new int[size];

        for (int i = 0; i < size; i++) {
            originalKeys[i] = scanner.nextInt();//Integer.parseInt(args[i + 2]);
            sortedKeys[i] = originalKeys[i];
        }
        Utils utils = new Utils();
        Algorithms algorithm = new Algorithms();

        if (size <= 50) {
            algorithm.setPrint(true);
        }
        int foundValue = 0;


        switch (mode) {
            case 1:
                foundValue = algorithm.select(sortedKeys, 0, sortedKeys.length - 1, position, utils);
                break;
            case 2:
                foundValue = algorithm.randomizedSelect(sortedKeys, 0, sortedKeys.length - 1, position, utils);
                break;
            case 3:
                foundValue = algorithm.experimentalSelect(sortedKeys, 0, sortedKeys.length - 1, position, 9, utils);
                break;
            case 4:
                // Position here means value
                foundValue = algorithm.binarySearch(sortedKeys, 0, sortedKeys.length - 1, position, utils);
                break;
            case 5:
                sortedKeys = algorithm.quickSortSelect(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
            case 6:
                sortedKeys = algorithm.dualSortSelect(sortedKeys, 0, sortedKeys.length - 1, utils);
                break;
        }

        if (mode == 4) {
            if (size <= 50) {
                System.out.println("####### ARRAY #######");
                utils.printArray(originalKeys);
            }
            System.out.println("Value to be found: " + position);
            if (foundValue == 1) {
                System.out.println("SUCCESS!");
            }
            else {
                System.out.println("FAILURE!");
            }
            System.out.println("#comparisons : " + utils.no_comparisons);
        }
        else if (mode == 5 || mode == 6) {
            if (size <= 50) {
                System.out.println("############### BEFORE ###############");
                utils.printArray(originalKeys);
                System.out.println("################ AFTER ###############");
                utils.printArray(sortedKeys);
            }
            if (utils.isSorted(sortedKeys)) {
                System.out.println("SUCCESS!");
            }
            else {
                System.out.println("FAILURE!");
            }
            System.out.println("#swaps       : " + utils.no_swaps);
            System.out.println("#comparisons : " + utils.no_comparisons);
        }
        else {
            if (size <= 50) {
                System.out.println("############### BEFORE SELECT ###############");
                utils.printArray(originalKeys);
                System.out.println("################ AFTER SELECT ###############");
                utils.printArray(sortedKeys);
                System.out.println("FOUND VALUE: " + foundValue + " (on position " + position + ")");
            }

            algorithm.mergeSort(sortedKeys, 0, sortedKeys.length - 1, utils);

            if (size <= 50) {
                System.out.println("############### AFTER SORTING ###############");
                utils.printArray(sortedKeys);
            }

            if (foundValue == sortedKeys[position - 1]) {
                System.out.println("SUCCESS!");
            }
            else {
                System.out.println("FAILURE!");
            }

            System.out.println("#swaps       : " + utils.no_swaps);
            System.out.println("#comparisons : " + utils.no_comparisons);
        }


        /*
        if (utils.isSorted(sortedKeys)) {
            System.out.println("SUCCESS!");
        }
        else {
            System.out.println("FAILURE!");
        }

         */
    }
}

