enum NodeColor {
    RED, BLACK;
}

class NodeRBT {
    int key;
    NodeColor color;
    NodeRBT left, right, parent;
    public NodeRBT(int key) {
        this.key = key;
        this.color = NodeColor.RED;
        this.left = this.right = this.parent = null;
    }
}

public class TreeRBT implements Tree {
    private NodeRBT root;
    private boolean isPrintReady = false;
    private int size;
    private char[] leftTrace;
    private char[] rightTrace;
    private boolean isNilNode = false;
    private Utils utils;
    public TreeRBT(Utils utils) {
        root = null;
        this.utils = utils;
    }

    public TreeRBT(int value, Utils utils) {
        root = new NodeRBT(value);
        this.utils = utils;
    }

    public void insert(int value) {
        NodeRBT current = root;
        NodeRBT parent = null;
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
        NodeRBT newNode = new NodeRBT(value);

        if (parent == null) {
            root = newNode;
        }
        else if (utils.compare(value, parent.key, 2)) {
            utils.registerSwap();
            parent.left = newNode;
        }
        else {
            utils.registerSwap();
            parent.right = newNode;
        }
        utils.registerSwap();
        newNode.parent = parent;

        fixAfterInsertion(newNode);
    }

    private void fixAfterInsertion(NodeRBT newNode) {
        NodeRBT parent = newNode.parent;
        // Case 1: Parent is NULL -> we inserted root
        if (parent == null) {
            newNode.color = NodeColor.BLACK;
            return;
        }
        // Case 2: Parent is black -> nothing to do
        if (parent.color == NodeColor.BLACK) {
            return;
        }
        NodeRBT grandParent = parent.parent;
        NodeRBT uncle = findUncle(newNode);
        // Case 3: Parent is red and Uncle is red -> recolor parent, grandparent and uncle
        if (uncle != null && uncle.color == NodeColor.RED) {
            parent.color = NodeColor.BLACK;
            uncle.color = NodeColor.BLACK;
            grandParent.color = NodeColor.RED;

            // Call recursively for grandparent, which is now red.
            // It might be root or have a red parent, in which case we need to fix more...
            fixAfterInsertion(grandParent);
        }
        // Case 4: Parent is red and Uncle is black (check variation)
        // Parent is left child of grandparent
        else if (parent == grandParent.left) {
            // Case 4a: New node is right child of his parent (LR case)
            if (newNode == parent.right) {
                rotateLeft(parent);

                // Let "parent" point to the new root node of the rotated sub-tree.
                // It will be recolored in the next step, which we're going to fall-through to.
                utils.registerSwap();
                parent = newNode;
            }

            // Case 5a: New node is now left child of his parent (LL case)
            rotateRight(grandParent);

            // Recolor original parent and grandparent
            parent.color = NodeColor.BLACK;
            grandParent.color = NodeColor.RED;
        }
        // Parent is right child of grandparent
        else {
            // Case 4b: New node is left child of his parent (RL case)
            if (newNode == parent.left) {
                rotateRight(parent);

                // Let "parent" point to the new root node of the rotated sub-tree.
                // It will be recolored in the next step, which we're going to fall-through to.
                utils.registerSwap();
                parent = newNode;
            }

            // Case 5b: New node is now right child of his parent (RR case)
            rotateLeft(grandParent);

            // Recolor original parent and grandparent
            parent.color = NodeColor.BLACK;
            grandParent.color = NodeColor.RED;
        }
    }

    public void delete(int value) {
        NodeRBT current = root;
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
        // At this point, "current" is the node to be deleted
        NodeRBT movedUpNode;
        NodeColor deletedNodeColor;
        // Node has zero or one child
        if (current.left == null || current.right == null) {
            movedUpNode = deleteNodeWithZeroOrOneChild(current);
            deletedNodeColor = current.color;
        }
        // Node has two children
        else {
            // Find minimum node of right subtree ("inorder successor" of current node)
            utils.registerSwap();
            NodeRBT successor = findMinimum(current.right);

            // Copy inorder successor's data to current node (keep its color!)
            current.key = successor.key;

            // Delete inorder successor just as we would delete a node with 0 or 1 child
            movedUpNode = deleteNodeWithZeroOrOneChild(successor);
            deletedNodeColor = successor.color;
        }
        if (deletedNodeColor == NodeColor.BLACK) {
            fixAfterDelete(movedUpNode);

            // Remove the temporary NIL node
            if (isNilNode) {
                isNilNode = false;
                replaceParentsChild(movedUpNode.parent, movedUpNode, null);
            }
        }
    }

    private boolean isBlack(NodeRBT node) {
        return node == null || node.color == NodeColor.BLACK;
    }

    private void fixAfterDelete(NodeRBT node) {
        // Case 1: Examined node is root, end of recursion
        if (node == root) {
            node.color = NodeColor.BLACK;
            return;
        }

        NodeRBT sibling = findSibling(node);

        // Case 2: Red sibling
        if (sibling.color == NodeColor.RED) {
            handleRedSibling(node, sibling);
            sibling = findSibling(node); // Get new sibling for fall-through to cases 3-6
        }

        // Cases 3+4: Black sibling with two black children
        if (isBlack(sibling.left) && isBlack(sibling.right)) {
            sibling.color = NodeColor.RED;

            // Case 3: Black sibling with two black children + red parent
            if (node.parent.color == NodeColor.RED) {
                node.parent.color = NodeColor.BLACK;
            }

            // Case 4: Black sibling with two black children + black parent
            else {
                fixAfterDelete(node.parent);
            }
        }

        // Case 5+6: Black sibling with at least one red child
        else {
            handleBlackSiblingWithAtLeastOneRedChild(node, sibling);
        }
    }

    private void handleBlackSiblingWithAtLeastOneRedChild(NodeRBT node, NodeRBT sibling) {
        boolean nodeIsLeftChild = node == node.parent.left;

        // Case 5: Black sibling with at least one red child + "outer nephew" is black
        // --> Recolor sibling and its child, and rotate around sibling
        if (nodeIsLeftChild && isBlack(sibling.right)) {
            sibling.left.color = NodeColor.BLACK;
            sibling.color = NodeColor.RED;
            rotateRight(sibling);
            utils.registerSwap();
            sibling = node.parent.right;
        }
        else if (!nodeIsLeftChild && isBlack(sibling.left)) {
            sibling.right.color = NodeColor.BLACK;
            sibling.color = NodeColor.RED;
            rotateLeft(sibling);
            utils.registerSwap();
            sibling = node.parent.left;
        }

        // Fall-through to case 6...

        // Case 6: Black sibling with at least one red child + "outer nephew" is red
        // --> Recolor sibling + parent + sibling's child, and rotate around parent
        sibling.color = node.parent.color;
        node.parent.color = NodeColor.BLACK;
        if (nodeIsLeftChild) {
            sibling.right.color = NodeColor.BLACK;
            rotateLeft(node.parent);
        }
        else {
            sibling.left.color = NodeColor.BLACK;
            rotateRight(node.parent);
        }
    }

    private void handleRedSibling(NodeRBT node, NodeRBT sibling) {
        // Recolour
        sibling.color = NodeColor.BLACK;
        node.parent.color = NodeColor.RED;
        // Rotate
        if (node == node.parent.left) {
            rotateLeft(node.parent);
        }
        else {
            rotateRight(node.parent);
        }
    }

    private NodeRBT deleteNodeWithZeroOrOneChild(NodeRBT node) {
        // Node has ONLY a left child --> replace by its left child
        if (node.left != null) {
            replaceParentsChild(node.parent, node, node.left);
            return node.left; // moved-up node
        }

        // Node has ONLY a right child --> replace by its right child
        else if (node.right != null) {
            replaceParentsChild(node.parent, node, node.right);
            return node.right; // moved-up node
        }

        // Node has no children -->
        // * node is red --> just remove it
        // * node is black --> replace it by a temporary NIL node (needed to fix the RBT rules)
        else {
            NodeRBT newChild;
            if (node.color == NodeColor.BLACK) {
                NodeRBT nilNode = new NodeRBT(-1);
                nilNode.color = NodeColor.BLACK;
                newChild = nilNode;
                isNilNode = true;
            }
            else {
                newChild = null;
            }
            replaceParentsChild(node.parent, node, newChild);
            return newChild;
        }
    }

    private NodeRBT findUncle(NodeRBT newNode) {
        NodeRBT parent = newNode.parent;
        if (parent == null) {
            return null;
        }
        NodeRBT grandparent = parent.parent;
        if (grandparent == null) {
            return null;
        }
        if (parent == grandparent.left) {
            return grandparent.right;
        }
        else {
            return grandparent.left;
        }
    }

    private NodeRBT findSibling(NodeRBT newNode) {
        NodeRBT parent = newNode.parent;
        if (parent == null) {
            return null;
        }
        if (parent.left == newNode) {
            return parent.right;
        }
        else {
            return parent.left;
        }
    }

    private NodeRBT findMinimum(NodeRBT node) {
        while (node.left != null) {
            node = node.left;
        }
        return node;
    }

    private void rotateLeft(NodeRBT node) {
        NodeRBT parent = node.parent;
        NodeRBT rightChild = node.right;

        utils.registerSwap();
        node.right = rightChild.left;
        if (rightChild.left != null) {
            utils.registerSwap();
            rightChild.left.parent = node;
        }

        utils.registerSwap();
        rightChild.left = node;
        utils.registerSwap();
        node.parent = rightChild;

        replaceParentsChild(parent, node, rightChild);
    }

    private void rotateRight(NodeRBT node) {
        NodeRBT parent = node.parent;
        NodeRBT leftChild = node.left;

        utils.registerSwap();
        node.left = leftChild.right;
        if (leftChild.right != null) {
            utils.registerSwap();
            leftChild.right.parent = node;
        }

        utils.registerSwap();
        leftChild.right = node;
        utils.registerSwap();
        node.parent = leftChild;

        replaceParentsChild(parent, node, leftChild);
    }

    void replaceParentsChild(NodeRBT parent, NodeRBT oldChild, NodeRBT newChild) {
        if (parent == null) {
            root = newChild;
        }
        else if (parent.left == oldChild) {
            utils.registerSwap();
            parent.left = newChild;
        }
        else if (parent.right == oldChild) {
            utils.registerSwap();
            parent.right = newChild;
        }
        else {
            return;
        }

        if (newChild != null) {
            utils.registerSwap();
            newChild.parent = parent;
        }
    }

    public int height() {
        return heightUsingRoot(root);
    }

    private int heightUsingRoot(NodeRBT root) {
        if (root == null) {
            return 0;
        }
        int leftHeight = heightUsingRoot(root.left);
        int rightHeight = heightUsingRoot(root.right);
        if (leftHeight > rightHeight)
            return leftHeight + 1;
        else
            return rightHeight + 1;
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

    private void printUsingRoot(NodeRBT root, int depth, char prefix) {
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
