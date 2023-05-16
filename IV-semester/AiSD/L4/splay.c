#include <stdlib.h>
#include "shared_functions.h"

struct NodeBST* createNode(int value) {
    struct NodeBST* temp = (struct NodeBST*)malloc(sizeof(struct NodeBST));
    temp->key = value;
    temp->left = temp->right = NULL;
    return temp;
}

struct NodeBST* rotateRight(struct NodeBST* node) {
    struct NodeBST* temp = node->left;
    node->left = temp->right;
    temp->right = node;

    return temp;
}

struct NodeBST* rotateLeft(struct NodeBST* node) {
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
            root = rotateRight(root);
        }
        else if (root->left->key < value) {
            root->left->right = splay(root->left->right, value);
            if (root->left->right != NULL)
                root->left = rotateLeft(root->left);
        }
        return (root->left == NULL) ? root : rotateRight(root);
    }
    else {
        if (root->right == NULL)
            return root;
        if (root->right->key > value) {
            root->right->left = splay(root->right->left, value);
            if (root->right->left != NULL)
                root->right = rotateRight(root->right);
        }
        else if (root->right->key < value) {
            root->right->right = splay(root->right->right, value);
            root = rotateLeft(root);
        }
        return (root->right == NULL) ? root : rotateLeft(root);
    }
}

struct NodeBST* insert(struct NodeBST* root, int value) {
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

struct NodeBST* delete(struct NodeBST* root, int value) {  
    struct NodeBST* temp;
    if (!root)
        return NULL;
  
    // Splay the given key
    root = splay(root, value);
  
    // If key is not present, then
    // return root
    if (value != root->key)
        return root;
  
    // If key is present
    // If left child of root does not exist
    // make root->right as root
    if (!root->left) {
        temp = root;
        root = root->right;
    }
  
    // Else if left child exits
    else {
        temp = root;
  
        /*Note: Since key == root->key,
        so after Splay(key, root->lchild),
        the tree we get will have no right child tree
        and maximum node in left subtree will get splayed*/
        // New root
        root = splay(root->left, value);
  
        // Make right child of previous root as
        // new root's right child
        root->right = temp->right;
    }
  
    // free the previous root node, that is,
    // the node containing the key
    free(temp);
  
    // return root of the new Splay Tree
    return root;
}

int main() {
    srand(time(NULL));
    struct NodeBST* root = NULL;
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
        printf("INSERT: [%d]\n\n", value);
        root = insert(root, value);
        printf("TREE:\n");
        print_BST(root, 0, '-');
        printf("\n\n");
    }
    
    for(int i=0; i<size; i++) {
        int value = rand() % (2 * size);
        printf("DELETE: [%d]\n\n", value);
        root = delete(root, value);
        printf("TREE:\n");
        print_BST(root, 0, '-');
        printf("\n\n");
    }
    
    free(left_trace);
    free(right_trace);
    free_subtree(&root);

    return 0;
}
