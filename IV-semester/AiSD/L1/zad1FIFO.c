#include <stdio.h>
#include <stdlib.h>
#define SIZE 100

struct Item {
	struct Item* next;
	int value;
};
struct Queue {
	struct Item *front, *back;
};

int amount_of_items = 0;
int new_item = 0;
void add_item(struct Queue* queue, int v);
void remove_item(struct Queue* queue);

int main() {
	srand(time(NULL));
	struct Queue* fifo_queue = (struct Queue*)malloc(sizeof(struct Queue*));
	fifo_queue->front = NULL;
	fifo_queue->back = NULL;
	printf("Adding 100 items:\n");
	for (int i = 1; i <= SIZE; i++) {
		new_item = rand() % 100;
		add_item(fifo_queue, new_item);
	}
	printf("\n");
	printf("Removing 100 items:\n");
	for (int i = 1; i <= SIZE; i++) {
		remove_item(fifo_queue);
	}
	printf("\n");
	free(fifo_queue);
	return 0;
}

void add_item(struct Queue* queue, int v) {
	if (amount_of_items + 1 > SIZE) {
		printf("FIFO queue is full!\n");
		return;
	}
	struct Item* new = (struct Item*)malloc(sizeof(struct Item));
	amount_of_items++;
	new->value = v;
	new->next = NULL;
	if (queue->back == NULL) {
		printf("%d ", new->value);
		queue->front = queue->back = new;
		return;
	}
	printf("%d ", new->value);
	queue->back->next = new;
	queue->back = new;
}

void remove_item(struct Queue* queue) {
	if (queue->front == NULL) {
		printf("FIFO queue is empty!\n");
		return;
	}
	amount_of_items--;
	printf("%d ", queue->front->value);
	struct Item* temp = queue->front;
	queue->front = queue->front->next;
	if (queue->front == NULL) {
		queue->back = NULL;
	}
	free(temp);
}

