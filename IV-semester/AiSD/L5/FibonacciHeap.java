import java.util.*;

class FibonacciHeap {

    public int operationComparisons = 0;
    private FibonacciNode minNode;
    private int size;

    public boolean isEmpty() {
        return minNode == null;
    }

    public int getSize() {
        return size;
    }

    public void insert(int key) {
        FibonacciNode newNode = new FibonacciNode(key);
        if (isEmpty()) {
            minNode = newNode;
        } else {
            newNode.left = minNode;
            newNode.right = minNode.right;
            minNode.right = newNode;
            newNode.right.left = newNode;
            operationComparisons++;
            if (newNode.key < minNode.key) {
                minNode = newNode;
            }
        }
        size++;
    }

    public void merge(FibonacciHeap heap1, FibonacciHeap heap2) {
        if (heap1.isEmpty()) {
            minNode = heap2.minNode;
        } else if (heap2.isEmpty()) {
            minNode = heap1.minNode;
        } else {
            FibonacciNode heap1Min = heap1.minNode;
            FibonacciNode heap2Min = heap2.minNode;

            FibonacciNode heap1MinRight = heap1Min.right;
            heap1Min.right = heap2Min.right;
            heap2Min.right.left = heap1Min;
            heap2Min.right = heap1MinRight;
            heap1MinRight.left = heap2Min;

            operationComparisons++;
            if (heap2Min.key < heap1Min.key) {
                minNode = heap2Min;
            } else {
                minNode = heap1Min;
            }
        }

        size = heap1.size + heap2.size;
    }

    public int getMinimum() {
        if (isEmpty()) {
            throw new NoSuchElementException("Heap is empty");
        }
        return minNode.key;
    }

    public int extractMinimum() {
        if (isEmpty()) {
            throw new NoSuchElementException("Heap is empty");
        }
        FibonacciNode extractedMin = minNode;
        if (extractedMin.child != null) {
            FibonacciNode child = extractedMin.child;
            do {
                FibonacciNode next = child.right;
                child.left = null;
                child.right = null;
                minNode.left.right = child;
                child.left = minNode.left;
                minNode.left = child;
                child.right = minNode;
                child.parent = null;
                child = next;
            } while (child != extractedMin.child);
        }
        extractedMin.left.right = extractedMin.right;
        extractedMin.right.left = extractedMin.left;
        if (extractedMin == extractedMin.right) {
            minNode = null;
        } else {
            minNode = extractedMin.right;
            consolidate();
        }
        size--;
        return extractedMin.key;
    }

    private void consolidate() {
        int maxDegree = (int) Math.floor(Math.log(size) / Math.log(1.618)) + 1;
        FibonacciNode[] degreeTable = new FibonacciNode[maxDegree];

        List<FibonacciNode> roots = new ArrayList<>();

        FibonacciNode currentNode = minNode;
        do {
            FibonacciNode current = currentNode;
            currentNode = currentNode.right;

            int degree = current.degree;
            while (degreeTable[degree] != null) {
                FibonacciNode other = degreeTable[degree];
                if (current.key > other.key) {
                    FibonacciNode temp = current;
                    current = other;
                    other = temp;
                }
                linkNodes(other, current);
                degreeTable[degree] = null;
                degree++;
            }

            degreeTable[degree] = current;
            roots.add(current);
        } while (currentNode != minNode);

        minNode = null;
        for (FibonacciNode root : roots) {
            if (minNode == null || root.key < minNode.key) {
                minNode = root;
            }
        }
    }



    private void linkNodes(FibonacciNode child, FibonacciNode parent) {
        child.left.right = child.right;
        child.right.left = child.left;
        child.parent = parent;
        if (parent.child == null) {
            parent.child = child;
            child.left = child;
            child.right = child;
        } else {
            child.left = parent.child;
            child.right = parent.child.right;
            parent.child.right = child;
            child.right.left = child;
        }
        parent.degree++;
        child.marked = false;
    }

    public void decreaseKey(FibonacciNode node, int newKey) {
        operationComparisons++;
        if (newKey > node.key) {
            throw new IllegalArgumentException("New key is greater than current key");
        }
        node.key = newKey;
        FibonacciNode parent = node.parent;
        operationComparisons++;
        if (parent != null && node.key < parent.key) {
            cut(node, parent);
            cascadingCut(parent);
        }
        operationComparisons++;
        if (node.key < minNode.key) {
            minNode = node;
        }
    }

    private void cut(FibonacciNode node, FibonacciNode parent) {
        node.left.right = node.right;
        node.right.left = node.left;
        parent.degree--;
        if (parent.child == node) {
            parent.child = node.right;
        }
        if (parent.degree == 0) {
            parent.child = null;
        }
        node.left = minNode;
        node.right = minNode.right;
        minNode.right = node;
        node.right.left = node;
        node.parent = null;
        node.marked = false;
    }

    private void cascadingCut(FibonacciNode node) {
        FibonacciNode parent = node.parent;
        if (parent != null) {
            if (!node.marked) {
                node.marked = true;
            } else {
                cut(node, parent);
                cascadingCut(parent);
            }
        }
    }

    private static class FibonacciNode {
        private int key;
        private int degree;
        private boolean marked;
        private FibonacciNode parent;
        private FibonacciNode child;
        private FibonacciNode left;
        private FibonacciNode right;

        public FibonacciNode(int key) {
            this.key = key;
            this.degree = 0;
            this.marked = false;
            this.parent = null;
            this.child = null;
            this.left = this;
            this.right = this;
        }
    }
}
