#include <stdlib.h>
#include "shared_functions.h"

struct NodeRBT* root = NULL;
bool isNilNode = false;

struct NodeRBT* findMinimum(struct NodeRBT* node);

void replaceParentsChild(struct NodeRBT* parent, struct NodeRBT* oldChild, struct NodeRBT* newChild) {
    if (parent == NULL) {
        root = newChild;
    } 
    else if (parent->left == oldChild) {
        parent->left = newChild;
    } 
    else if (parent->right == oldChild) {
        parent->right = newChild;
    } 
    else {
        return;
    }

    if (newChild != NULL) {
        newChild->parent = parent;
    }
}

void rotateLeft(struct NodeRBT* node) {
    struct NodeRBT* parent = node->parent;
    struct NodeRBT* rightChild = node->right;

    node->right = rightChild->left;
    if (rightChild->left != NULL) {
        rightChild->left->parent = node;
    }

    rightChild->left = node;
    node->parent = rightChild;

    replaceParentsChild(parent, node, rightChild);
}

void rotateRight(struct NodeRBT* node) {
    struct NodeRBT* parent = node->parent;
    struct NodeRBT* leftChild = node->left;

    node->left = leftChild->right;
    if (leftChild->right != NULL) {
        leftChild->right->parent = node;
    }

    leftChild->right = node;
    node->parent = leftChild;

    replaceParentsChild(parent, node, leftChild);
}

struct NodeRBT* findUncle(struct NodeRBT* parent) {
    struct NodeRBT* grandparent = parent->parent;
    if (grandparent->left == parent) {
        return grandparent->right;
    }
    else if (grandparent->right == parent) {
        return grandparent->left;
    }
    return NULL;
}

void fixAfterInsertion(struct NodeRBT* node) {
    struct NodeRBT* parent = node->parent;
    // Case 1: Parent is NULL -> we inserted root
    if (parent == NULL) {
        node->color = BLACK;
        return;
    }

    // Parent is black --> nothing to do
    if (parent->color == BLACK) {
        return;
    }

    struct NodeRBT* grandparent = parent->parent;
    struct NodeRBT* uncle = findUncle(parent);

    // Case 2: Uncle is red -> recolor parent, grandparent and uncle
    if (uncle != NULL && uncle->color == RED) {
        parent->color = BLACK;
        grandparent->color = RED;
        uncle->color = BLACK;

        // Call recursively for grandparent, which is now red.
        // It might be root or have a red parent, in which case we need to fix more...
        fixAfterInsertion(grandparent);
    }
    // Parent is left child of grandparent
    else if (parent == grandparent->left) {
    // Case 3a: Uncle is black and node is left->right "inner child" of its grandparent
        if (node == parent->right) {
            rotateLeft(parent);

        // Let "parent" point to the new root node of the rotated sub-tree.
        // It will be recolored in the next step, which we're going to fall-through to.
            parent = node;
        }

        // Case 4a: Uncle is black and node is left->left "outer child" of its grandparent
        rotateRight(grandparent);

        // Recolor original parent and grandparent
        parent->color = BLACK;
        grandparent->color = RED;
    }

    // Parent is right child of grandparent
    else {
        // Case 3b: Uncle is black and node is right->left "inner child" of its grandparent
        if (node == parent->left) {
        rotateRight(parent);

        // Let "parent" point to the new root node of the rotated sub-tree.
        // It will be recolored in the next step, which we're going to fall-through to.
        parent = node;
        }

        // Case 4b: Uncle is black and node is right->right "outer child" of its grandparent
        rotateLeft(grandparent);

        // Recolor original parent and grandparent
        parent->color = BLACK;
        grandparent->color = RED;
    }
}

void insert(int value) {
    struct NodeRBT* node = root;
    struct NodeRBT* parent = NULL;

    while (node != NULL) {
        parent = node;
        if (value < node->key) {
            node = node->left;
        }
        else if (value > node->key) {
            node = node->right;
        }
        else {
            return;
        }
    }

    struct NodeRBT* temp = (struct NodeRBT*)malloc(sizeof(struct NodeRBT));
    temp->key = value;
    temp->color = RED;

    if (parent == NULL) {
        root = temp;
    } 
    else if (value < parent->key) {
        parent->left = temp;
    } 
    else {
        parent->right = temp;
    }
    temp->parent = parent;

    fixAfterInsertion(temp);
}

struct NodeRBT* getSibling(struct NodeRBT* node) {
    struct NodeRBT* parent = node->parent;
    if (node == parent->left) {
        return parent->right;
    }
    else if (node == parent->right) {
        return parent->left;
    } 
    return NULL;
}

bool isBlack(struct NodeRBT* node) {
    return node == NULL || node->color == BLACK;
}

void handleRedSibling(struct NodeRBT* node, struct NodeRBT* sibling) {
    // Recolor...
    sibling->color = BLACK;
    node->parent->color = RED;
    // ... and rotate
    if (node == node->parent->left) {
        rotateLeft(node->parent);
    } 
    else {
        rotateRight(node->parent);
    }
}

void handleBlackSiblingWithAtLeastOneRedChild(struct NodeRBT* node, struct NodeRBT* sibling) {
    bool nodeIsLeftChild = node == node->parent->left;

    // Case 5: Black sibling with at least one red child + "outer nephew" is black
    // --> Recolor sibling and its child, and rotate around sibling
    if (nodeIsLeftChild && isBlack(sibling->right)) {
        sibling->left->color = BLACK;
        sibling->color = RED;
        rotateRight(sibling);
        sibling = node->parent->right;
    } 
    else if (!nodeIsLeftChild && isBlack(sibling->left)) {
        sibling->right->color = BLACK;
        sibling->color = RED;
        rotateLeft(sibling);
        sibling = node->parent->left;
    }

    // Fall-through to case 6...

    // Case 6: Black sibling with at least one red child + "outer nephew" is red
    // --> Recolor sibling + parent + sibling's child, and rotate around parent
    sibling->color = node->parent->color;
    node->parent->color = BLACK;
    if (nodeIsLeftChild) {
        sibling->right->color = BLACK;
        rotateLeft(node->parent);
    } 
    else {
        sibling->left->color = BLACK;
        rotateRight(node->parent);
    }
}

void fixAfterDelete(struct NodeRBT* node) {
    // Case 1: Examined node is root, end of recursion
    if (node == root) {
        node->color = BLACK;
        return;
    }

    struct NodeRBT* sibling = getSibling(node);

    // Case 2: Red sibling
    if (sibling->color == RED) {
        handleRedSibling(node, sibling);
        sibling = getSibling(node); // Get new sibling for fall-through to cases 3-6
    }

    // Cases 3+4: Black sibling with two black children
    if (isBlack(sibling->left) && isBlack(sibling->right)) {
        sibling->color = RED;

        // Case 3: Black sibling with two black children + red parent
        if (node->parent->color == RED) {
            node->parent->color = BLACK;
        }

        // Case 4: Black sibling with two black children + black parent
        else {
        fixAfterDelete(node->parent);
        }
    }

    // Case 5+6: Black sibling with at least one red child
    else {
        handleBlackSiblingWithAtLeastOneRedChild(node, sibling);
    }
}

struct NodeRBT* deleteNodeWithZeroOrOneChild(struct NodeRBT* node) {
    // Node has ONLY a left child --> replace by its left child
    if (node->left != NULL) {
        replaceParentsChild(node->parent, node, node->left);
        return node->left; // moved-up node
    }

    // Node has ONLY a right child --> replace by its right child
    else if (node->right != NULL) {
        replaceParentsChild(node->parent, node, node->right);
        return node->right; // moved-up node
    }

    // Node has no children -->
    // * node is red --> just remove it
    // * node is black --> replace it by a temporary NIL node (needed to fix the R-B rules)
    else {
        struct NodeRBT* newChild;
        if (node->color == BLACK) {
            struct NodeRBT* nilNode = (struct NodeRBT*)malloc(sizeof(struct NodeRBT));
            nilNode->color = BLACK;
            nilNode->key = 0;
            newChild = nilNode;
            isNilNode = true;
        }
        else {
            newChild = NULL;
        }
        replaceParentsChild(node->parent, node, newChild);
        return newChild;
    }
}

struct NodeRBT* findMinimum(struct NodeRBT* node) {  
    while (node->left != NULL) {
        node = node->left;
    }
    return node; 
}  

void delete(int value) {
    struct NodeRBT* node = root; 
    // Find the node to be deleted
    while (node != NULL && node->key != value) {
    // Traverse the tree to the left or right depending on the key
        if (value < node->key) {
            node = node->left;
        } 
        else {
            node = node->right;
        }
    }
    // Node not found?
    if (node == NULL) {
        return;
    }

    // At this point, "node" is the node to be deleted

    // In this variable, we'll store the node at which we're going to start to fix the R-B
    // properties after deleting a node.
    struct NodeRBT* movedUpNode;
    enum Color deletedNodeColor;

    // Node has zero or one child
    if (node->left == NULL || node->right == NULL) {
        movedUpNode = deleteNodeWithZeroOrOneChild(node);
        deletedNodeColor = node->color;
    }

    // Node has two children
    else {
        // Find minimum node of right subtree ("inorder successor" of current node)
        struct NodeRBT* inOrderSuccessor = findMinimum(node->right);

        // Copy inorder successor's data to current node (keep its color!)
        node->key = inOrderSuccessor->key;

        // Delete inorder successor just as we would delete a node with 0 or 1 child
        movedUpNode = deleteNodeWithZeroOrOneChild(inOrderSuccessor);
        deletedNodeColor = inOrderSuccessor->color;
    }

    if (deletedNodeColor == BLACK) {
        fixAfterDelete(movedUpNode);

        // Remove the temporary NIL node
        if (isNilNode) {
            isNilNode = false;
            replaceParentsChild(movedUpNode->parent, movedUpNode, NULL);
        }
    } 
}

int main() {
    srand(time(NULL));
    int size;
    scanf("%d", &size);
    if (size <= 0) {
        printf("Wrong size!: %d\n", size);
        return -1;
    }
	int* input = calloc(size, sizeof(int));
    for (int i = 0; i < size; i++) {
		scanf("%d", &input[i]);
	}

    left_trace= malloc( size*sizeof(char) );
    right_trace= malloc( size*sizeof(char) );
    for(int i=0; i<size; i++){
        left_trace[i]=' ';
        left_trace[i]=' ';
    }
    printf("\n");

    for(int i=0; i<size; i++) {
        int value = input[i];
        inOrderTraversal(root);
        printf("INSERT: [%d]\n\n", value);
        //if (i == 0) {
        //    root = insert(root, value);
        //}
        //else {
            insert(value);
        //}
        printf("TREE:\n");
        print_RBT(root, 0, '-');
        printf("\n\n");
    }

    
    for(int i=0; i<size; i++) {
        int value = rand() % (2 * size);
        printf("DELETE: [%d]\n\n", value);
        delete(value);
        printf("TREE:\n");
        print_RBT(root, 0, '-');
        printf("\n\n");
    }
    

    free(left_trace);
    free(right_trace);
    free_subtreeRBT(&root);

    return 0;
}