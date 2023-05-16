#include <stdio.h>
#include <stdbool.h>

int comparisons = 0;
int swaps = 0;
char* left_trace;
char* right_trace;

struct NodeBST {
    int key;
    struct NodeBST* left;
    struct NodeBST* right;
};

enum Color {
	RED,
	BLACK
};

struct NodeRBT {
	int key;
	enum Color color;
	struct NodeRBT* left;
	struct NodeRBT* right;
	struct NodeRBT* parent;
};

int height(struct NodeBST* root) {
    if (root == NULL)
        return 0;
    int left_height = height(root->left);
    int right_height = height(root->right);
    if (left_height > right_height)
        return left_height + 1;
    else
        return right_height + 1;
}

void inOrderTraversal(struct NodeRBT* root) {
    if (root != NULL) {
        inOrderTraversal(root->left);
        printf("%d ", root->key);
        inOrderTraversal(root->right);
    }
}

void print_RBT(struct NodeRBT* root, int depth, char prefix){
  	if( root == NULL ) return;
  	if( root->left != NULL ){
    	print_RBT(root->left, depth+1, '/');
  	}
  	if(prefix == '/') left_trace[depth-1]='|';
  	if(prefix == '\\') right_trace[depth-1]=' ';
  	if( depth==0) printf("-");
  	if( depth>0) printf(" ");
  	for(int i=0; i<depth-1; i++)
    	if( left_trace[i]== '|' || right_trace[i]=='|' ) {
      		printf("| ");
    	} 
		else {
      		printf("  ");
    	}
  	if( depth>0 ) printf("%c-", prefix);
  	printf("[%d]\n", root->key);
  	left_trace[depth]=' ';
  	if( root->right != NULL ){
    	right_trace[depth]='|';
    	print_RBT(root->right, depth+1, '\\');
  	}
}

void print_BST(struct NodeBST* root, int depth, char prefix){
  	if( root == NULL ) return;
  	if( root->left != NULL ){
    	print_BST(root->left, depth+1, '/');
  	}
  	if(prefix == '/') left_trace[depth-1]='|';
  	if(prefix == '\\') right_trace[depth-1]=' ';
  	if( depth==0) printf("-");
  	if( depth>0) printf(" ");
  	for(int i=0; i<depth-1; i++)
    	if( left_trace[i]== '|' || right_trace[i]=='|' ) {
      		printf("| ");
    	} 
		else {
      		printf("  ");
    	}
  	if( depth>0 ) printf("%c-", prefix);
  	printf("[%d]\n", root->key);
  	left_trace[depth]=' ';
  	if( root->right != NULL ){
    	right_trace[depth]='|';
    	print_BST(root->right, depth+1, '\\');
  	}
}

void free_subtreeRBT(struct NodeRBT** root) {
  	if( *root == NULL ) return;
  	if( (*root)->left != NULL )
    	free_subtreeRBT( &(*root)->left );
  	if( (*root)->right != NULL )
    	free_subtreeRBT( &(*root)->right );
  	free(*root);
  	*root=NULL;  
}

void free_subtree(struct NodeBST** root) {
  	if( *root == NULL ) return;
  	if( (*root)->left != NULL )
    	free_subtree( &(*root)->left );
  	if( (*root)->right != NULL )
    	free_subtree( &(*root)->right );
  	free(*root);
  	*root=NULL;  
}

void swap(int *a, int *b) {
  int temp = *a;
  *a = *b;
  *b = temp;
  swaps++;
}

