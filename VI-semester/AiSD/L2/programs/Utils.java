public class Utils {
    int no_comparisons = 0;
    int no_swaps = 0;
    public boolean compare(int a, int b, int mode) {
        no_comparisons++;
        return switch (mode) {
            case 1 -> a > b;
            case 2 -> a < b;
            case 3 -> a >= b;
            case 4 -> a <= b;
            case 5 -> a == b;
            default -> throw new IllegalArgumentException();
        };
    }
    public void registerSwap() {
        no_swaps++;
    }
    public void swap(int[] keys, int firstIndex, int secondIndex) {
        no_swaps++;
        int temp = keys[firstIndex];
        keys[firstIndex] = keys[secondIndex];
        keys[secondIndex] = temp;
    }

    public void rotate3(int[] keys, int firstIndex, int secondIndex, int thirdIndex) {
        int temp = keys[firstIndex];
        keys[firstIndex] = keys[secondIndex];
        keys[secondIndex] = keys[thirdIndex];
        keys[thirdIndex] = temp;
        no_swaps += 3;
    }

    public void printArray(int[] array) {
        for (int j : array) {
            if (j < 10) {
                System.out.print("0");
            }
            System.out.print(j + "  ");
        }
        System.out.println();
    }

    public boolean isSorted(int[] array) {
        for (int i = 0; i < array.length - 1; i++) {
            if (array[i] > array[i + 1]) {
                return false;
            }
        }
        return true;
    }

    public void resetCounters() {
        no_comparisons = 0;
        no_swaps = 0;
    }
}
