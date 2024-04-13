import java.util.ArrayList;
import java.util.Stack;

public class Algorithms {
    public static boolean print = false;
    public int[] insertionSort(int[] keys, int size, Utils utils) {
        int key, j;
        for (int i = 1; i < size; i++) {
            key = keys[i];
            j = i - 1;
            while (j >= 0 && utils.compare(keys[j], key, 1)) {
                utils.registerSwap();
                keys[j + 1] = keys[j];
                j--;
            }
            keys[j + 1] = key;
            if (print) {
                if (i < 10) {
                    System.out.println("############### STEP 0" + i + " ###############");
                }
                else {
                    System.out.println("############### STEP " + i + " ###############");
                }
                utils.printArray(keys);
            }
        }
        return keys;
    }

    public int[] hybridInsertionSort(int[] keys, int low, int high, Utils utils) {
        int key, j;
        for (int i = low + 1; i <= high; i++) {
            key = keys[i];
            j = i;
            while (j > low && utils.compare(keys[j - 1], key, 1)) {
                utils.registerSwap();
                keys[j] = keys[j - 1];
                j--;
            }
            keys[j] = key;
            if (print) {
                if (i < 10) {
                    System.out.println("############### STEP 0" + i + " ###############");
                }
                else {
                    System.out.println("############### STEP " + i + " ###############");
                }
                utils.printArray(keys);
            }
        }
        return keys;
    }

    public int partition(int[] keys, int low, int high, Utils utils) {
        int pivot = keys[high];
        int i = low - 1;

        for (int j = low; j < high; j++) {
            if (utils.compare(keys[j], pivot, 4)) {
                i++;
                utils.swap(keys, i, j);
            }
        }

        utils.swap(keys, i + 1, high);

        if (print) {
            System.out.println("############### PARTITION FROM INDICES: " + i + " - " + high + " ###############");
            utils.printArray(keys);
        }

        return (i + 1);
    }

    public int[] quickSort(int[] keys, int low, int high, Utils utils) {
        if (low < high) {
            int pivot = partition(keys, low, high, utils);
            quickSort(keys, low, pivot - 1, utils);
            quickSort(keys, pivot + 1, high, utils);
        }

        return keys;
    }

    public int[] hybridSort(int[] keys, int low, int high, Utils utils, int threshold) {
        if (low < high) {
            if (high - low < threshold) {
                hybridInsertionSort(keys, low, high, utils);
            }
            else {
                int pivot = partition(keys, low, high, utils);
                hybridSort(keys, low, pivot - 1, utils, threshold);
                hybridSort(keys, pivot + 1, high, utils, threshold);
            }
        }

        return keys;
    }

    public int[] dualSortPartition(int[] keys, int low, int high, Utils utils) {
        int i = low + 1;
        int from_left_index = low + 1;
        int from_right_index = high - 1;
        int d = 0;
        int left_pivot = keys[low];
        int right_pivot = keys[high];

        while (from_left_index <= from_right_index) {
            if (d >= 0) {
                if (utils.compare(keys[from_left_index], left_pivot, 2)) {
                    utils.swap(keys, from_left_index, i);
                    i++;
                    from_left_index++;
                    d++;
                }
                else {
                    if (utils.compare(keys[from_left_index], right_pivot, 2)) {
                        from_left_index++;
                    }
                    else {
                        utils.swap(keys, from_left_index, from_right_index);
                        from_right_index--;
                        d--;
                    }
                }
            }
            else {

                if (utils.compare(keys[from_right_index], right_pivot, 1)) {
                    from_right_index--;
                    d--;
                }
                else {
                    if (utils.compare(keys[from_right_index], left_pivot, 2)) {
                        utils.rotate3(keys, from_right_index, from_left_index, i);
                        i++;
                        d++;
                    }
                    else {
                        utils.swap(keys, from_left_index, from_right_index);
                    }
                    from_left_index++;
                }

            }
        }

        utils.swap(keys, low, i - 1);
        utils.swap(keys, high, from_right_index + 1);

        int[] receivedPivots = new int[2];

        receivedPivots[0] = i - 1;
        receivedPivots[1] = from_right_index + 1;

        if (print) {
            System.out.println("############### PARTITION ON INDICES: [" + low + "] --- [" + receivedPivots[0] +
                    "] --- [" + receivedPivots[1] + "] --- [" + high  + "] ###############");
            utils.printArray(keys);
        }

        return receivedPivots;
    }

    public int[] dualSort(int[] keys, int low, int high, Utils utils) {
        if (low < high) {
            if (utils.compare(keys[low], keys[high], 1)) {
                utils.swap(keys, low, high);
            }
            int[] receivedPivots = dualSortPartition(keys, low, high, utils);

            dualSort(keys, low, receivedPivots[0] - 1, utils);
            dualSort(keys, receivedPivots[0] + 1, receivedPivots[1] - 1, utils);
            dualSort(keys, receivedPivots[1] + 1, high, utils);
        }

        return keys;
    }

    public int[] mergeWithoutArrayLists(int[] array, int left, int right, int mid, Utils utils) {
        int currentFirstSize = mid - left + 1;
        int currentSecondSize = right - mid;

        int[] leftArray = new int[currentFirstSize];
        int[] rightArray = new int[currentSecondSize];

        System.arraycopy(array, left, leftArray, 0, currentFirstSize);
        System.arraycopy(array, mid + 1, rightArray, 0, currentSecondSize);

        int currentGlobalIndex = left;
        int currentLeftIndex = 0;
        int currentRightIndex = 0;

        while (currentLeftIndex < currentFirstSize && currentRightIndex < currentSecondSize) {
            if (utils.compare(leftArray[currentLeftIndex], rightArray[currentRightIndex], 1)) {
                array[currentGlobalIndex] = rightArray[currentRightIndex];
                currentRightIndex++;
            }
            else {
                array[currentGlobalIndex] = leftArray[currentLeftIndex];
                currentLeftIndex++;
            }
            currentGlobalIndex++;
            utils.registerSwap();
        }

        while (currentLeftIndex < currentFirstSize) {
            array[currentGlobalIndex] = leftArray[currentLeftIndex];
            currentGlobalIndex++;
            currentLeftIndex++;
            utils.registerSwap();
        }

        while (currentRightIndex < currentSecondSize) {
            array[currentGlobalIndex] = rightArray[currentRightIndex];
            currentGlobalIndex++;
            currentRightIndex++;
            utils.registerSwap();
        }

        return array;
    }

    public int[] mergeSort(int[] array, int left, int right, Utils utils) {
        if (left < right) {
            int mid = left + (right - left) / 2;
            mergeSort(array, left, mid, utils);
            mergeSort(array, mid + 1, right, utils);
            mergeWithoutArrayLists(array, left, right, mid, utils);

            if (print) {
                System.out.println("############### MERGE FROM INDICES: " + left + " - " + right + " ###############");
                utils.printArray(array);
            }
        }
        return array;
    }

    public int[][] findAllRuns(int[] array, Utils utils) {
        int startIndex = 0;
        int lastIndex = startIndex + 1;
        ArrayList<int[]> runsArrayList = new ArrayList<>();
        while (lastIndex < array.length) {
            int iteratingStartingIndex = startIndex;
            while (array.length - 1 >= lastIndex && utils.compare(array[iteratingStartingIndex], array[lastIndex], 4)) {
                iteratingStartingIndex++;
                lastIndex++;
            }
            lastIndex--;
            int[] foundRun = new int[lastIndex - startIndex + 1];
            System.arraycopy(array, startIndex, foundRun, 0, foundRun.length);
            runsArrayList.add(foundRun);
            startIndex = lastIndex + 1;
            lastIndex = startIndex + 1;
        }
        int[][] runs = new int[runsArrayList.size()][];
        for (int i = 0; i < runsArrayList.size(); i++) {
            runs[i] = runsArrayList.get(i);
        }

        return runs;
    }

    public int[] mergeTwoArrays(int[] array1, int[] array2, Utils utils) {
        int currentFirstSize = array1.length;
        int currentSecondSize = array2.length;

        int[] leftArray = new int[currentFirstSize];
        int[] rightArray = new int[currentSecondSize];

        System.arraycopy(array1, 0, leftArray, 0, currentFirstSize);
        System.arraycopy(array2, 0, rightArray, 0, currentSecondSize);

        int currentGlobalIndex = 0;
        int currentLeftIndex = 0;
        int currentRightIndex = 0;

        int[] mergedArrays = new int[currentFirstSize + currentSecondSize];

        while (currentLeftIndex < currentFirstSize && currentRightIndex < currentSecondSize) {
            if (utils.compare(leftArray[currentLeftIndex], rightArray[currentRightIndex], 1)) {
                mergedArrays[currentGlobalIndex] = rightArray[currentRightIndex];
                currentRightIndex++;
            }
            else {
                mergedArrays[currentGlobalIndex] = leftArray[currentLeftIndex];
                currentLeftIndex++;
            }
            currentGlobalIndex++;
            utils.registerSwap();
        }

        while (currentLeftIndex < currentFirstSize) {
            mergedArrays[currentGlobalIndex] = leftArray[currentLeftIndex];
            currentGlobalIndex++;
            currentLeftIndex++;
            utils.registerSwap();
        }

        while (currentRightIndex < currentSecondSize) {
            mergedArrays[currentGlobalIndex] = rightArray[currentRightIndex];
            currentGlobalIndex++;
            currentRightIndex++;
            utils.registerSwap();
        }

        return mergedArrays;
    }

    public int[] mergeMultipleArrays(int[][] arrays, Utils utils) {
        if (arrays.length == 1) {
            return arrays[0];
        }
        if (arrays.length == 2) {
            int[] result = mergeTwoArrays(arrays[0], arrays[1], utils);
            if (print) {
                System.out.println("############### MERGE ###############");
                utils.printArray(result);
            }

            return result;
        }
        int leftSize = arrays.length / 2;
        int rightSize = (int) Math.ceil((double) arrays.length / 2.0);
        int[][] left = new int[leftSize][];
        int[][] right = new int[rightSize][];
        for (int i = 0; i < leftSize; i++) {
            left[i] = new int[arrays[i].length];
            System.arraycopy(arrays[i], 0, left[i], 0, arrays[i].length);
        }
        for (int i = 0; i < rightSize; i++) {
            right[i] = new int[arrays[leftSize + i].length];
            System.arraycopy(arrays[leftSize + i], 0, right[i], 0, arrays[leftSize + i].length);
        }

        int[] leftArray = mergeMultipleArrays(left, utils);
        int[] rightArray = mergeMultipleArrays(right, utils);

        int[] result = mergeTwoArrays(leftArray, rightArray, utils);

        if (print) {
            System.out.println("############### MERGE ###############");
            utils.printArray(result);
        }

        return result;
    }

    public int[] improvisedSort(int[] array, Utils utils) {
        int[][] runs = findAllRuns(array, utils);

        return mergeMultipleArrays(runs, utils);
    }

    public int findNextRun(int[] array, int startIndex, Utils utils) {
        int lastIndex = startIndex + 1;
        while (array.length - 1 >= lastIndex && utils.compare(array[startIndex], array[lastIndex], 4)) {
            startIndex++;
            lastIndex++;
        }

        return lastIndex - 1;
    }

    public int power(int[] run, int startIndex, int endIndex, int n) {
        int rangeOfRun = run[1] - run[0];
        int rangeOfArray = endIndex - startIndex;

        double startOfInterval = (run[0] + 0.5 * rangeOfRun - 1) / (double) n;
        double endOfInterval = (startIndex + 0.5 * rangeOfArray - 1) / (double) n;;

        int power = 0;
        while (Math.floor(startOfInterval * Math.pow(2, power)) == Math.floor(endOfInterval * Math.pow(2, power))) {
            power++;
        }

        return power;
    }

    public int[] powerSort(int[] array, Utils utils) {
        int startIndex = 0;
        Stack<int[]> runs = new Stack<>();
        int endIndex = findNextRun(array, startIndex, utils);
        int[] run = {startIndex, endIndex, 0};
        runs.push(run);
        startIndex = endIndex + 1;

        while (startIndex < array.length) {
            endIndex = findNextRun(array, startIndex, utils);
            int power = power(runs.peek(), startIndex, endIndex, array.length);
            int[] foundRun = {startIndex, endIndex, power};
            runs.push(foundRun);
            while (!runs.isEmpty() && power <= runs.peek()[2]) {
                int[] secondRun = runs.pop();
                int[] firstRun = runs.pop();
                int mid = firstRun[0] + (secondRun[1] - firstRun[0]) / 2;
                mergeWithoutArrayLists(array, firstRun[0], secondRun[1], mid, utils);
            }
            int[] nextRun = {startIndex, endIndex, power};
            runs.push(nextRun);
            startIndex = endIndex + 1;
        }

        while (runs.size() >= 2) {
            int[] firstRun = runs.pop();
            int[] secondRun = runs.pop();
            int mid = firstRun[0] + (secondRun[1] - firstRun[0]) / 2;
            mergeWithoutArrayLists(array, firstRun[0], secondRun[1], mid, utils);
        }

        return array;
    }

    public void setPrint(boolean value) {
        print = value;
    }
}
