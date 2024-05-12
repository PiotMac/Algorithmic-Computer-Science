import java.util.LinkedList;
import java.util.Queue;

class NodeBST {
    int key;
    NodeBST left, right, parent;
    public NodeBST(int key) {
        this.key = key;
        left = right = parent = null;
    }
}

public class TreeBST implements Tree {
    private NodeBST root;
    private boolean isPrintReady = false;
    private int size;
    private char[] leftTrace;
    private char[] rightTrace;
    private final Utils utils;

    public TreeBST(Utils utils) {
        root = null;
        this.utils = utils;
    }

    public TreeBST(int value, Utils utils) {
        root = new NodeBST(value);
        this.utils = utils;
    }

    public void insert(int value) {
        NodeBST current = root;
        NodeBST parent = null;

        while (current != null) {
            parent = current;
            if (utils.compare(value, current.key, 2)) {
                utils.registerSwap();
                current = current.left;
            }
            else if (utils.compare(value, current.key, 1)) {
                utils.registerSwap();
                current = current.right;
            }
            else {
                return;
            }
        }

        NodeBST newNode = new NodeBST(value);

        utils.registerSwap();
        if (parent == null) {
            root = newNode;
        }
        else if (utils.compare(value, parent.key, 2)) {
            parent.left = newNode;
        }
        else {
            parent.right = newNode;
        }
        utils.registerSwap();
        newNode.parent = parent;
    }

    public void delete(int value) {
        deleteFromChosenRoot(root, value);
    }

    private void deleteFromChosenRoot(NodeBST root, int value) {
        NodeBST current = root;

        // Traverse the tree to find the node with the specified key
        while (current != null) {
            if (utils.compare(current.key, value, 5)) {
                break;
            }
            else if (utils.compare(value, current.key, 2)) {
                utils.registerSwap();
                current = current.left;
            }
            else {
                utils.registerSwap();
                current = current.right;
            }
        }
        // No such node found to be deleted
        if (current == null) {
            return;
        }

        if (current.left == null && current.right == null) {
            if (current == this.root) {
                this.root = null;
                return;
            }
            if (current == current.parent.left) {
                current.parent.left = null;
            }
            else {
                current.parent.right = null;
            }
            return;
        }
        if (current.left == null) {
            utils.registerSwap();
            if (current == this.root) {
                this.root = current.right;
                this.root.parent = null;
                return;
            }
            if (current == current.parent.left) {
                current.parent.left = current.right;
            }
            else {
                current.parent.right = current.right;
            }
            current.right.parent = current.parent;

            return;
        }
        if (current.right == null) {
            utils.registerSwap();
            if (current == this.root) {
                this.root = current.left;
                this.root.parent = null;
                return;
            }
            if (current == current.parent.left) {
                current.parent.left = current.left;
            }
            else {
                current.parent.right = current.left;
            }
            current.left.parent = current.parent;

            return;
        }

        utils.registerSwap();
        int minValue = findMinimumValue(current.right);
        deleteFromChosenRoot(current.right, minValue);
        current.key = minValue;
    }

    private int findMinimumValue(NodeBST root) {
        int min = root.key;
        while (root.left != null) {
            utils.registerSwap();
            min = root.left.key;
            root = root.left;
        }
        return min;
    }

    public int height() {
        if (root == null)
            return 0;

        Queue<NodeBST> queue = new LinkedList<>();
        queue.add(root);
        int height = 0;

        while (!queue.isEmpty()) {
            int nodeCount = queue.size();
            height++;

            while (nodeCount > 0) {
                NodeBST tempNode = queue.poll();

                if (tempNode.left != null)
                    queue.add(tempNode.left);

                if (tempNode.right != null)
                    queue.add(tempNode.right);

                nodeCount--;
            }
        }
        return height;
    }

    public void print() {
        if (isPrintReady) {
            printUsingRoot(root, 0, '-');
            System.out.println();
            for (int i = 0; i < size; i++) {
                leftTrace[i] = ' ';
                rightTrace[i] = ' ';
            }
        }
        else {
            System.out.println("Set print size first!");
        }
    }

    private void printUsingRoot(NodeBST root, int depth, char prefix) {
        if (root == null)
            return;

        if (root.left != null) {
            printUsingRoot(root.left, depth + 1, '/');
        }

        if (prefix == '/') {
            leftTrace[depth - 1] = '|';
        }

        if (prefix == '\\') {
            rightTrace[depth - 1] = ' ';
        }

        if (depth == 0) {
            System.out.print("-");
        }

        if (depth > 0) {
            System.out.print(" ");
        }

        for (int i = 0; i < depth - 1; i++) {
            if (leftTrace[i] == '|' || rightTrace[i] == '|') {
                System.out.print("| ");
            } else {
                System.out.print("  ");
            }
        }

        if (depth > 0) {
            System.out.print(prefix + "-");
        }

        System.out.println("[" + root.key + "]");

        leftTrace[depth] = ' ';

        if (root.right != null) {
            rightTrace[depth] = '|';
            printUsingRoot(root.right, depth + 1, '\\');
        }
    }

    public void setPrintSize(int size) {
        isPrintReady = true;
        this.size = size;
        leftTrace = new char[size];
        rightTrace = new char[size];
        for (int i = 0; i < size; i++) {
            leftTrace[i] = ' ';
            rightTrace[i] = ' ';
        }
    }
}
