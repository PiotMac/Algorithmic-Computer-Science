#include "shared_functions.h"

struct NodeRBT* rootRBT = NULL;
bool isNilNode = false;

struct NodeBST* createNode(int value) {
    struct NodeBST* temp = (struct NodeBST*)malloc(sizeof(struct NodeBST));
    temp->key = value;
    temp->left = temp->right = NULL;
    return temp;
}

struct NodeBST* insertBST(struct NodeBST* node, int value) {
    if (node == NULL)
        return createNode(value);
    if (value < node->key)
        node->left = insertBST(node->left, value);
    else if (value > node->key)
        node->right = insertBST(node->right, value);
    return node;
}

struct NodeBST* findMinimumBST(struct NodeBST* node) {  
    while (node->left != NULL) {
        node = node->left;
    }
    return node; 
}

struct NodeBST* deleteBST(struct NodeBST* node, int value) {  
    if (node == NULL)  
        return node;  
    if (value > node->key)  
        node->right = deleteBST(node->right, value);  
    else if (value < node->key)  
        node->left = deleteBST(node->left, value);  
    else {  
        if (node->left == NULL && node->right == NULL){  
            free(node);  
            return NULL;  
        }  
        else if (node->left == NULL || node->right == NULL){  
            struct NodeBST *temp;  
            if (node->left == NULL)  
                temp = node->right;  
            else  
                temp = node->left;  
            free(node);  
            return temp;  
        }  
        else {  
            struct NodeBST *temp = findMinimumBST(node->right);  
            node->key = temp->key;  
            node->right = deleteBST(node->right, temp->key);  
        }  
    }  
    return node;  
}

struct NodeBST* rotateRightSplay(struct NodeBST* node) {
    struct NodeBST* temp = node->left;
    node->left = temp->right;
    temp->right = node;

    return temp;
}

struct NodeBST* rotateLeftSplay(struct NodeBST* node) {
    struct NodeBST* temp = node->right;
    node->right = temp->left;
    temp->left = node;

    return temp;
}

struct NodeBST* splay(struct NodeBST* root, int value) {
    if (root == NULL || root->key == value)
        return root;
 
    if (root->key > value) {
        if (root->left == NULL)
            return root;
        if (root->left->key > value) {
            root->left->left = splay(root->left->left, value);
            root = rotateRightSplay(root);
        }
        else if (root->left->key < value) {
            root->left->right = splay(root->left->right, value);
            if (root->left->right != NULL)
                root->left = rotateLeftSplay(root->left);
        }
        return (root->left == NULL) ? root : rotateRightSplay(root);
    }
    else {
        if (root->right == NULL)
            return root;
        if (root->right->key > value) {
            root->right->left = splay(root->right->left, value);
            if (root->right->left != NULL)
                root->right = rotateRightSplay(root->right);
        }
        else if (root->right->key < value) {
            root->right->right = splay(root->right->right, value);
            root = rotateLeftSplay(root);
        }
        return (root->right == NULL) ? root : rotateLeftSplay(root);
    }
}

struct NodeBST* insertSplay(struct NodeBST* root, int value) {
    if (root == NULL)
        return createNode(value);
 
    root = splay(root, value);
 
    if (root->key == value)
        return root;
 
    struct NodeBST* temp = createNode(value);
    if (root->key > value) {
        temp->right = root;
        temp->left = root->left;
        root->left = NULL;
    }
    else {
        temp->left = root;
        temp->right = root->right;
        root->right = NULL;
    }
    return temp;
}

struct NodeBST* deleteSplay(struct NodeBST* root, int value) {  
    struct NodeBST* temp;
    if (!root)
        return NULL;
  
    root = splay(root, value);
  
    if (value != root->key)
        return root;
  
    if (!root->left) {
        temp = root;
        root = root->right;
    }
    else {
        temp = root;
        root = splay(root->left, value);
        root->right = temp->right;
    }
  
    free(temp);
    return root;
}

struct NodeRBT* findMinimumRBT(struct NodeRBT* node) {  
    while (node->left != NULL) {
        node = node->left;
    }
    return node; 
}

void replaceParentsChild(struct NodeRBT* parent, struct NodeRBT* oldChild, struct NodeRBT* newChild) {
    if (parent == NULL) {
        rootRBT = newChild;
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

void rotateLeftRBT(struct NodeRBT* node) {
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

void rotateRightRBT(struct NodeRBT* node) {
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

void fixAfterInsertionRBT(struct NodeRBT* node) {
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
        fixAfterInsertionRBT(grandparent);
    }
    // Parent is left child of grandparent
    else if (parent == grandparent->left) {
    // Case 3a: Uncle is black and node is left->right "inner child" of its grandparent
        if (node == parent->right) {
            rotateLeftRBT(parent);

        // Let "parent" point to the new root node of the rotated sub-tree.
        // It will be recolored in the next step, which we're going to fall-through to.
            parent = node;
        }

        // Case 4a: Uncle is black and node is left->left "outer child" of its grandparent
        rotateRightRBT(grandparent);

        // Recolor original parent and grandparent
        parent->color = BLACK;
        grandparent->color = RED;
    }

    // Parent is right child of grandparent
    else {
        // Case 3b: Uncle is black and node is right->left "inner child" of its grandparent
        if (node == parent->left) {
        rotateRightRBT(parent);

        // Let "parent" point to the new root node of the rotated sub-tree.
        // It will be recolored in the next step, which we're going to fall-through to.
        parent = node;
        }

        // Case 4b: Uncle is black and node is right->right "outer child" of its grandparent
        rotateLeftRBT(grandparent);

        // Recolor original parent and grandparent
        parent->color = BLACK;
        grandparent->color = RED;
    }
}

void insertRBT(int value) {
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
        rootRBT = temp;
    } 
    else if (value < parent->key) {
        parent->left = temp;
    } 
    else {
        parent->right = temp;
    }
    temp->parent = parent;

    fixAfterInsertionRBT(temp);
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
    sibling->color = BLACK;
    node->parent->color = RED;
    if (node == node->parent->left) {
        rotateLeftRBT(node->parent);
    } 
    else {
        rotateRightRBT(node->parent);
    }
}

void handleBlackSiblingWithAtLeastOneRedChild(struct NodeRBT* node, struct NodeRBT* sibling) {
    bool nodeIsLeftChild = node == node->parent->left;

    if (nodeIsLeftChild && isBlack(sibling->right)) {
        sibling->left->color = BLACK;
        sibling->color = RED;
        rotateRightRBT(sibling);
        sibling = node->parent->right;
    } 
    else if (!nodeIsLeftChild && isBlack(sibling->left)) {
        sibling->right->color = BLACK;
        sibling->color = RED;
        rotateLeftRBT(sibling);
        sibling = node->parent->left;
    }

    sibling->color = node->parent->color;
    node->parent->color = BLACK;
    if (nodeIsLeftChild) {
        sibling->right->color = BLACK;
        rotateLeftRBT(node->parent);
    } 
    else {
        sibling->left->color = BLACK;
        rotateRightRBT(node->parent);
    }
}

void fixAfterDeleteRBT(struct NodeRBT* node) {
    if (node == rootRBT) {
        node->color = BLACK;
        return;
    }

    struct NodeRBT* sibling = getSibling(node);

    if (sibling->color == RED) {
        handleRedSibling(node, sibling);
        sibling = getSibling(node);
    }

    if (isBlack(sibling->left) && isBlack(sibling->right)) {
        sibling->color = RED;

        if (node->parent->color == RED) {
            node->parent->color = BLACK;
        }

        else {
        fixAfterDeleteRBT(node->parent);
        }
    }

    else {
        handleBlackSiblingWithAtLeastOneRedChild(node, sibling);
    }
}

struct NodeRBT* deleteNodeWithZeroOrOneChild(struct NodeRBT* node) {
    if (node->left != NULL) {
        replaceParentsChild(node->parent, node, node->left);
        return node->left;
    }

    else if (node->right != NULL) {
        replaceParentsChild(node->parent, node, node->right);
        return node->right;
    }

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

void deleteRBT(int value) {
    struct NodeRBT* node = rootRBT; 
    while (node != NULL && node->key != value) {
        if (value < node->key) {
            node = node->left;
        } 
        else {
            node = node->right;
        }
    }
    if (node == NULL) {
        return;
    }

    struct NodeRBT* movedUpNode;
    enum Color deletedNodeColor;

    if (node->left == NULL || node->right == NULL) {
        movedUpNode = deleteNodeWithZeroOrOneChild(node);
        deletedNodeColor = node->color;
    }
    else {
        struct NodeRBT* inOrderSuccessor = findMinimumRBT(node->right);

        node->key = inOrderSuccessor->key;
        movedUpNode = deleteNodeWithZeroOrOneChild(inOrderSuccessor);
        deletedNodeColor = inOrderSuccessor->color;
    }

    if (deletedNodeColor == BLACK) {
        fixAfterDeleteRBT(movedUpNode);

        if (isNilNode) {
            isNilNode = false;
            replaceParentsChild(movedUpNode->parent, movedUpNode, NULL);
        }
    } 
}
