import java.util.ArrayList;
import java.util.List;

public class FibonacciHeap {
    public int operationComparisons = 0;

    private static class FibonacciTree {
        int value;
        List<FibonacciTree> child;
        int order;

        FibonacciTree(int value) {
            this.value = value;
            this.child = new ArrayList<>();
            this.order = 0;
        }

        void addAtEnd(FibonacciTree t) {
            child.add(t);
            order++;
        }
    }

    public List<FibonacciTree> trees;
    private FibonacciTree least;
    private int count;

    public FibonacciHeap() {
        this.trees = new ArrayList<>();
        this.least = null;
        this.count = 0;
    }

    public void insert(int value) {
        FibonacciTree newTree = new FibonacciTree(value);
        trees.add(newTree);
        operationComparisons++;
        if (least == null || value < least.value) {
            least = newTree;
        }
        count++;
    }

    public Integer getMin() {
        if (least == null) {
            return null;
        }
        return least.value;
    }

    public Integer extractMin() {
        FibonacciTree smallest = least;
        if (smallest != null) {
            for (FibonacciTree child : smallest.child) {
                trees.add(child);
            }
            trees.remove(smallest);
            if (trees.isEmpty()) {
                least = null;
            } else {
                least = trees.get(0);
                consolidate();
            }
            count--;
            return smallest.value;
        }
        return null;
    }

    private void consolidate() {
        int maxOrder = (int) (Math.log(count) / Math.log(2)) + 1;
        List<FibonacciTree> aux = new ArrayList<>();
        for (int i = 0; i <= maxOrder; i++) {
            aux.add(null);
        }

        while (!trees.isEmpty()) {
            FibonacciTree x = trees.get(0);
            int order = x.order;
            trees.remove(x);

            while (aux.get(order) != null) {
                FibonacciTree y = aux.get(order);
                operationComparisons++;
                if (x.value > y.value) {
                    FibonacciTree temp = x;
                    x = y;
                    y = temp;
                }
                x.addAtEnd(y);
                aux.set(order, null);
                order++;
            }
            aux.set(order, x);
        }

        least = null;
        for (FibonacciTree k : aux) {
            if (k != null) {
                trees.add(k);
                operationComparisons++;
                if (least == null || k.value < least.value) {
                    least = k;
                }
            }
        }
    }

    public void merge(FibonacciHeap otherHeap) {
        trees.addAll(otherHeap.trees);
        operationComparisons++;
        if (least == null || (otherHeap.least != null && otherHeap.least.value < least.value)) {
            least = otherHeap.least;
        }
        count += otherHeap.count;
    }
/*
    public static void main(String[] args) {
        FibonacciHeap fibonacciHeap = new FibonacciHeap();
        fibonacciHeap.insert(1);
        fibonacciHeap.insert(2);
        fibonacciHeap.insert(3);
        fibonacciHeap.insert(4);
        fibonacciHeap.insert(5);
        fibonacciHeap.insert(6);
        fibonacciHeap.extractMin();
        fibonacciHeap.extractMin();
        fibonacciHeap.extractMin();
        fibonacciHeap.extractMin();
        fibonacciHeap.extractMin();
        fibonacciHeap.extractMin();
    }

 */
}
