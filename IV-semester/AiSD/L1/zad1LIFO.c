#include <stdio.h>
#include <stdlib.h>
#define SIZE 100

struct Item {
	struct Item* below;
	int value;
};
struct Stack {
	struct Item *top;
};

int amount_of_items = 0;
int new_item = 0;
void push(struct Stack* stack, int v);
void pop(struct Stack* stack);

int main() {
	srand(time(NULL));
	struct Stack* stack = (struct Stack*)malloc(sizeof(struct Stack*));
	stack->top = NULL;
	printf("Adding 100 items:\n");
	for (int i = 1; i <= SIZE; i++) {
		new_item = rand() % 100;
		push(stack, new_item);
	}
	printf("\n");
	printf("Removing 100 items:\n");
	for (int i = 1; i <= SIZE; i++) {
		pop(stack);
	}
	printf("\n");
	free(stack);
	return 0;
}

void push(struct Stack* stack, int v) {
	if (amount_of_items + 1 > SIZE) {
		printf("Stack is full!\n");
		return;
	}
	struct Item* new = (struct Item*)malloc(sizeof(struct Item));
	amount_of_items++;
	new->value = v;
	new->below = stack->top;
	printf("%d ", new->value);
	stack->top = new;
}

void pop(struct Stack* stack) {
	if (stack->top == NULL) {
		printf("Stack is empty!\n");
		return;
	}
	amount_of_items--;
	printf("%d ", stack->top->value);
	struct Item* temp = stack->top;
	stack->top = stack->top->below;
	free(temp);
}

