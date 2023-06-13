import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.NoSuchElementException;

public class BinomialHeap {
    public int operationComparisons = 0;

    private static class BinomialTree {
        int key;
        List<BinomialTree> children;
        int order;

        BinomialTree(int key) {
            this.key = key;
            this.children = new ArrayList<>();
            this.order = 0;
        }

        void addAtEnd(BinomialTree t) {
            children.add(t);
            order++;
        }
    }

    public List<BinomialTree> trees;

    public BinomialHeap() {
        this.trees = new ArrayList<>();
    }

    public int extractMin() {
        if (trees.isEmpty()) {
            throw new NoSuchElementException("Heap is empty");
        }

        BinomialTree smallestNode = trees.get(0);
        for (BinomialTree tree : trees) {
            operationComparisons++;
            if (tree.key < smallestNode.key) {
                smallestNode = tree;
            }
        }

        trees.remove(smallestNode);

        BinomialHeap h = new BinomialHeap();
        h.trees = smallestNode.children;
        merge(h);

        return smallestNode.key;
    }

    public int getMin() {
        if (trees.isEmpty()) {
            throw new NoSuchElementException("Heap is empty");
        }

        int least = trees.get(0).key;
        for (BinomialTree tree : trees) {
            operationComparisons++;
            if (tree.key < least) {
                least = tree.key;
            }
        }

        return least;
    }

    private void combineRoots(BinomialHeap h) {
        trees.addAll(h.trees);
        trees.sort(Comparator.comparingInt(tree -> tree.order));
    }

    public void merge(BinomialHeap h) {
        combineRoots(h);
        if (trees.isEmpty()) {
            return;
        }

        int i = 0;
        while (i < trees.size() - 1) {
            BinomialTree current = trees.get(i);
            BinomialTree after = trees.get(i + 1);
            if (current.order == after.order) {
                if (i + 1 < trees.size() - 1 && trees.get(i + 2).order == after.order) {
                    BinomialTree afterAfter = trees.get(i + 2);
                    operationComparisons++;
                    if (after.key < afterAfter.key) {
                        after.addAtEnd(afterAfter);
                        trees.remove(i + 2);
                    } else {
                        afterAfter.addAtEnd(after);
                        trees.remove(i + 1);
                    }
                } else {
                    operationComparisons++;
                    if (current.key < after.key) {
                        current.addAtEnd(after);
                        trees.remove(i + 1);
                    } else {
                        after.addAtEnd(current);
                        trees.remove(i);
                    }
                }
            }
            i++;
        }
    }

    public void insert(int key) {
        BinomialHeap g = new BinomialHeap();
        g.trees.add(new BinomialTree(key));
        merge(g);
    }
/*
    public static void main(String[] args) {
        BinomialHeap binomialHeap = new BinomialHeap();
        binomialHeap.insert(1);
        binomialHeap.insert(2);
        binomialHeap.insert(3);
        binomialHeap.insert(4);
        binomialHeap.insert(5);
        binomialHeap.insert(6);
        binomialHeap.extractMin();
        binomialHeap.extractMin();
        binomialHeap.extractMin();
        binomialHeap.extractMin();
        binomialHeap.extractMin();
        binomialHeap.extractMin();
    }*/
}

