import java.util.LinkedList;
import java.util.Queue;

class NodeSplay {
    int key;
    NodeSplay left, right, parent;
    public NodeSplay(int key) {
        this.key = key;
        left = right = parent = null;
    }
}

public class TreeSplay implements Tree {
    private NodeSplay root;
    private boolean isPrintReady = false;
    private int size;
    private char[] leftTrace;
    private char[] rightTrace;
    private final Utils utils;

    public TreeSplay(Utils utils) {
        root = null;
        this.utils = utils;
    }

    public TreeSplay(int value, Utils utils) {
        root = new NodeSplay(value);
        this.utils = utils;
    }

    private void rotateLeft(NodeSplay node) {
        NodeSplay rightChild = node.right;

        utils.registerSwap();
        node.right = rightChild.left;

        if (rightChild.left != null) {
            utils.registerSwap();
            rightChild.left.parent = node;
        }
        utils.registerSwap();
        rightChild.parent = node.parent;
        if (node.parent == null) {
            utils.registerSwap();
            this.root = rightChild;
        }
        else if (node.parent.left == node) {
            utils.registerSwap();
            node.parent.left = rightChild;
        }
        else {
            utils.registerSwap();
            node.parent.right = rightChild;
        }

        utils.registerSwap();
        rightChild.left = node;

        utils.registerSwap();
        node.parent = rightChild;
    }

    private void rotateRight(NodeSplay node) {
        NodeSplay leftChild = node.left;

        utils.registerSwap();
        node.left = leftChild.right;

        if (leftChild.right != null) {
            utils.registerSwap();
            leftChild.right.parent = node;
        }
        utils.registerSwap();
        leftChild.parent = node.parent;
        if (node.parent == null) {
            utils.registerSwap();
            this.root = leftChild;
        }
        else if (node.parent.right == node) {
            utils.registerSwap();
            node.parent.right = leftChild;
        }
        else {
            utils.registerSwap();
            node.parent.left = leftChild;
        }

        utils.registerSwap();
        leftChild.right = node;

        utils.registerSwap();
        node.parent = leftChild;
    }

    private void splay(NodeSplay node) {
        while (node.parent != null) {
            if (node.parent.parent == null) {
                if (node == node.parent.left) {
                    // Zig rotation
                    rotateRight(node.parent);
                }
                else {
                    // Zag rotation
                    rotateLeft(node.parent);
                }
            }
            else if (node == node.parent.left && node.parent == node.parent.parent.left) {
                // Zig-Zig rotation
                rotateRight(node.parent.parent);
                rotateRight(node.parent);
            }
            else if (node == node.parent.right && node.parent == node.parent.parent.right) {
                // Zag-Zag rotation
                rotateLeft(node.parent.parent);
                rotateLeft(node.parent);
            }
            else if (node == node.parent.right && node.parent == node.parent.parent.left) {
                // Zig-Zag rotation
                rotateLeft(node.parent);
                rotateRight(node.parent);
            }
            else {
                // zag-zig rotation
                rotateRight(node.parent);
                rotateLeft(node.parent);
            }
        }
    }

    public void insert(int value) {
        NodeSplay current = root;
        NodeSplay parent = null;

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

        NodeSplay newNode = new NodeSplay(value);

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

        splay(newNode);
    }

    public void delete(int value) {
        deleteFromChosenRoot(root, value);
    }

    private void deleteFromChosenRoot(NodeSplay node, int value) {
        NodeSplay x = null;
        NodeSplay current = node;
        while (current != null){
            if (utils.compare(current.key, value, 5)) {
                utils.registerSwap();
                x = current;
                break;
            }

            if (utils.compare(current.key, value, 4)) {
                utils.registerSwap();
                current = current.right;
            } else {
                utils.registerSwap();
                current = current.left;
            }
        }

        // Couldn't find key in the tree
        if (x == null) {
            return;
        }
        // Split operation
        splay(x);
        if (x.left == null) {
            utils.registerSwap();
            this.root = x.right;
            if (this.root != null) {
                this.root.parent = null;
            }
            return;
        }
        if (x.right == null) {
            utils.registerSwap();
            this.root = x.left;
            if (this.root != null) {
                this.root.parent = null;
            }
            return;
        }

        NodeSplay leftMaximum = maximum(x.left);
        splay(leftMaximum);

        utils.registerSwap();
        leftMaximum.right = x.right;

        if (leftMaximum.right != null) {
            utils.registerSwap();
            leftMaximum.right.parent = leftMaximum;
        }

        utils.registerSwap();
        this.root = leftMaximum;
        this.root.parent = null;
    }

    // Find the node with the maximum key
    private NodeSplay maximum(NodeSplay node) {
        while (node.right != null) {
            node = node.right;
        }
        return node;
    }

    public int height() {
        if (root == null)
            return 0;

        Queue<NodeSplay> queue = new LinkedList<>();
        queue.add(root);
        int height = 0;

        while (!queue.isEmpty()) {
            int nodeCount = queue.size();
            height++;

            while (nodeCount > 0) {
                NodeSplay tempNode = queue.poll();

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

    private void printUsingRoot(NodeSplay root, int depth, char prefix) {
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
