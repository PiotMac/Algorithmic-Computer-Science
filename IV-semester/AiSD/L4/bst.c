#include <stdlib.h>
#include "shared_functions.h"

struct NodeBST* createNode(int value) {
    struct NodeBST* temp = (struct NodeBST*)malloc(sizeof(struct NodeBST));
    temp->key = value;
    temp->left = temp->right = NULL;
    return temp;
}

struct NodeBST* insert(struct NodeBST* node, int value) {
    if (node == NULL)
        return createNode(value);
    if (value < node->key)
        node->left = insert(node->left, value);
    else if (value > node->key)
        node->right = insert(node->right, value);
    return node;
}

struct NodeBST* find_minimum(struct NodeBST* root) {  
    if (root == NULL)  
        return NULL;  
    else if (root->left != NULL)  
        return find_minimum(root->left);  
    return root;  
}  

struct NodeBST* delete(struct NodeBST* node, int value) {  
    if (node == NULL)  
        return node;  
    if (value > node->key)  
        node->right = delete(node->right, value);  
    else if (value < node->key)  
        node->left = delete(node->left, value);  
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
            struct NodeBST *temp = find_minimum(node->right);  
            node->key = temp->key;  
            node->right = delete(node->right, temp->key);  
        }  
    }  
    return node;  
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
        if (i == 0) {
            root = insert(root, value);
        }
        else {
            insert(root, value);
        }
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
