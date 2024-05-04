import java.security.SecureRandom;

public class Algorithms {
    public static boolean print = false;

    public void insertionSort(int[] keys, int low, int high, Utils utils) {
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
        }
    }

    public int[] merge(int[] array, int left, int right, int mid, Utils utils) {
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
            merge(array, left, right, mid, utils);
        }
        return array;
    }

    public void divideAndSort5Elements(int[] array, int p, int q, Utils utils) {
        int numberOfParts = (int) Math.ceil((q - p + 1) / 5.0);
        int loopEnder = 0;
        int i = p;
        while (loopEnder < numberOfParts) {
            insertionSort(array, i, Math.min(i + 4, q), utils);
            loopEnder++;
            i += 5;
        }
    }

    public int partitionSelect(int[] array, int low, int high, int pivot, Utils utils) {
        int i;
        for (i = low; i <= high; i++) {
            if (utils.compare(array[i], pivot, 5)) {
                break;
            }
        }
        utils.swap(array, i, high);
        i = low;
        for (int j = low; j < high; j++) {
            if (utils.compare(array[j], pivot, 4)) {
                utils.swap(array, i, j);
                i++;
            }
        }
        utils.swap(array, i, high);

        return i;
    }

    public int select(int[] array, int p, int q, int k, Utils utils) {
        if (q == p) {
            return array[p];
        }

        divideAndSort5Elements(array, p, q, utils);
        int numberOfParts = (int) Math.ceil((q - p + 1) / 5.0);

        int[] medians = new int[numberOfParts];
        for (int i = 0; i < numberOfParts; i++) {
            if (i == numberOfParts - 1 && (q - p + 1) % 5 != 0) {
                int sizeLeft = (q - p + 1) % 5;
                int midIndex = (int) Math.ceil((double) sizeLeft / 2.0) - 1;
                medians[i] = array[p + i * 5 + midIndex];
            }
            else {
                medians[i] = array[p + i * 5 + 2];
            }
        }

        int medianOfMedians = select(medians, 0, medians.length - 1, (int) Math.ceil(medians.length / 2.0), utils);

        int pivotIndex = partitionSelect(array, p, q, medianOfMedians, utils);

        if (print) {
            System.out.println("############### PARTITION FROM INDICES: " + p + " - " + q + " ###############");
            utils.printArray(array);
        }

        if (k - 1 == pivotIndex) {
            return array[pivotIndex];
        } else if (k - 1 < pivotIndex) {
            return select(array, p, pivotIndex - 1, k, utils);
        } else {
            return select(array, pivotIndex + 1, q, k, utils);
        }
    }

    public int randomPartition(int[] keys, int low, int high, Utils utils) {
        SecureRandom random = new SecureRandom();
        int randomIndex = random.nextInt(high - low + 1) + low;
        utils.swap(keys, randomIndex, high);

        int pivot = keys[high];
        int i = low - 1;

        for (int j = low; j < high; j++) {
            if (utils.compare(keys[j], pivot, 4)) {
                i++;
                utils.swap(keys, i, j);
            }
        }

        utils.swap(keys, i + 1, high);

        return (i + 1);
    }

    public int randomizedSelect(int[] array, int p, int q, int k, Utils utils) {
        if (p == q) {
            return array[p];
        }
        int pivotIndex = randomPartition(array, p, q, utils);

        if (print) {
            System.out.println("############### PARTITION FROM INDICES: " + p + " - " + q + " ###############");
            utils.printArray(array);
        }

        if (pivotIndex == k - 1) {
            return array[pivotIndex];
        }
        else if (pivotIndex < k - 1) {
            return randomizedSelect(array, pivotIndex + 1, q, k, utils);
        }
        else {
            return randomizedSelect(array, p, pivotIndex - 1, k, utils);
        }
    }

    public void divideAndSortNElements(int[] array, int p, int q, int n, Utils utils) {
        int numberOfParts = (int) Math.ceil((double) (q - p + 1) / n);
        int loopEnder = 0;
        int i = p;
        while (loopEnder < numberOfParts) {
            insertionSort(array, i, Math.min(i + n - 1, q), utils);
            loopEnder++;
            i += n;
        }
    }

    public int experimentalSelect(int[] array, int p, int q, int k, int sizeOfBlock, Utils utils) {
        if (q == p) {
            return array[p];
        }

        divideAndSortNElements(array, p, q, sizeOfBlock, utils);
        int numberOfParts = (int) Math.ceil((q - p + 1) / (double) sizeOfBlock);

        int[] medians = new int[numberOfParts];
        for (int i = 0; i < numberOfParts; i++) {
            if (i == numberOfParts - 1 && (q - p + 1) % sizeOfBlock != 0) {
                int sizeLeft = (q - p + 1) % sizeOfBlock;
                int midIndex = (int) Math.ceil((double) sizeLeft / 2.0) - 1;
                medians[i] = array[p + i * sizeOfBlock + midIndex];
            }
            else {
                medians[i] = array[p + i * sizeOfBlock + ((sizeOfBlock - 1) / 2)];
            }
        }

        int medianOfMedians = select(medians, 0, medians.length - 1, (int) Math.ceil(medians.length / 2.0), utils);

        int pivotIndex = partitionSelect(array, p, q, medianOfMedians, utils);

        if (k - 1 == pivotIndex) {
            return array[pivotIndex];
        } else if (k - 1 < pivotIndex) {
            return experimentalSelect(array, p, pivotIndex - 1, k, sizeOfBlock, utils);
        } else {
            return experimentalSelect(array, pivotIndex + 1, q, k, sizeOfBlock, utils);
        }
    }

    int binarySearch(int[] array, int left, int right, int x, Utils utils) {
        if (right >= left) {
            int mid = left + (right - left) / 2;

            // If the element is present at the middle itself
            if (utils.compare(array[mid], x, 5)) {
                return 1;
            }

            // If element is smaller than mid, then it can only be present in left subarray
            if (utils.compare(array[mid], x, 1)) {
                return binarySearch(array, left, mid - 1, x, utils);
            }

            // Else the element can only be present in right subarray
            return binarySearch(array, mid + 1, right, x, utils);
        }

        // If element is not present in array
        return 0;
    }

    public int partitionQuickSelect(int[] keys, int low, int high, int pivot, Utils utils) {
        int i;

        for (i = low; i <= high; i++) {
            if (utils.compare(keys[i], pivot, 5)) {
                break;
            }
        }
        utils.swap(keys, i, high);

        i = low - 1;
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

    public int[] quickSortSelect(int[] keys, int low, int high, Utils utils) {
        if (low < high) {
            boolean changed = false;
            if (print) {
                print = false;
                changed = true;
            }
            int middle = select(keys, low, high, ((high - low) / 2 + low) + 1, utils);
            if (changed) {
                print = true;
            }
            int pivot = partitionQuickSelect(keys, low, high, middle, utils);
            quickSortSelect(keys, low, pivot - 1, utils);
            quickSortSelect(keys, pivot + 1, high, utils);
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

    public int[] dualSortSelectPartition(int[] keys, int low, int high, Utils utils) {
        int from_left_index = low + 1;
        int from_right_index = high - 1;
        int d = 0;

        int i;
        int leftPivot;
        int rightPivot;

        if (high - low <= 2) {
            leftPivot = keys[low];
            rightPivot = keys[high];
        }
        else {
            boolean changed = false;
            if (print) {
                print = false;
                changed = true;
            }
            leftPivot = select(keys, low, high, (high - low) / 3 + low + 1, utils);
            rightPivot = select(keys, low, high, 2 * (high - low) / 3 + low + 1, utils);
            if (changed) {
                print = true;
            }

            for (i = low; i <= high; i++) {
                if (utils.compare(keys[i], leftPivot, 5)) {
                    break;
                }
            }
            utils.swap(keys, i, low);

            int j;
            for (j = high; j >= low; j--) {
                if (utils.compare(keys[j], rightPivot, 5)) {
                    break;
                }
            }
            utils.swap(keys, j, high);
        }

        i = low + 1;

        while (from_left_index <= from_right_index) {
            if (d >= 0) {
                if (utils.compare(keys[from_left_index], leftPivot, 2)) {
                    utils.swap(keys, from_left_index, i);
                    i++;
                    from_left_index++;
                    d++;
                }
                else {
                    if (utils.compare(keys[from_left_index], rightPivot, 2)) {
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

                if (utils.compare(keys[from_right_index], rightPivot, 1)) {
                    from_right_index--;
                    d--;
                }
                else {
                    if (utils.compare(keys[from_right_index], leftPivot, 2)) {
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

    public int[] dualSortSelect(int[] keys, int low, int high, Utils utils) {
        if (low < high) {
            if (utils.compare(keys[low], keys[high], 1)) {
                utils.swap(keys, low, high);
            }
            int[] receivedPivots = dualSortSelectPartition(keys, low, high, utils);

            dualSortSelect(keys, low, receivedPivots[0] - 1, utils);
            dualSortSelect(keys, receivedPivots[0] + 1, receivedPivots[1] - 1, utils);
            dualSortSelect(keys, receivedPivots[1] + 1, high, utils);
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

    public void setPrint(boolean value) {
        print = value;
    }
}
