#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define SIZE 10000

struct Item {
	struct Item* next;
	int value;
};
struct List {
	struct Item *head;
};

clock_t start, end;
double cpu_time_used;
int new_item = 0;
void push_front(struct List* list, int v);
void push_index(struct List* list, int v, unsigned int index);
void push_back(struct List* list, int v);
void pop_front(struct List* list);
void pop_index(struct List* list, unsigned int index);
void pop_back(struct List* list);
struct List* merge(struct List* list1, struct List* list2);
double check_element(struct List* list, unsigned int index);

int main() {
	if (SIZE == 0) {
		printf("Size zero has been declared! No action can be done...\n");
		return 0;
	}
	srand(time(NULL));
	struct List* list = (struct List*)malloc(sizeof(struct List*));
	list->head = NULL;
	struct List* list1 = (struct List*)malloc(sizeof(struct List*));
	list1->head = NULL;
	for (int i = 1; i <= SIZE; i++) {
		new_item = rand() % 100;
		push_front(list, new_item);
	}
	double test = 0.0;
	for (int i = 1; i <= 10; i++) {
		test += check_element(list, 99);
	}
	printf("TEST 100 = %f\n", test / 10.0);

	test = 0.0;
	for (int i = 1; i <= 10; i++) {
		test += check_element(list, 999);
	}
	printf("TEST 1000 = %f\n", test / 10.0);

	test = 0.0;
	for (int i = 1; i <= 10; i++) {
		test += check_element(list, 4999);
	}
	printf("TEST 5000 = %f\n", test / 10.0);
	
	test = 0.0;
	for (int i = 1; i <= 10; i++) {
		test += check_element(list, 9999);
	}
	printf("TEST 10000 = %f\n", test / 10.0);
	
	test = 0.0;
	new_item = rand() % SIZE;
	for (int i = 1; i <= 10; i++) {
		test += check_element(list, new_item - 1);
	}
	printf("TEST %d = %f\n", new_item, test / 10.0);
	
	for (int i = 1; i <= SIZE; i++) {
		new_item = rand() % 100;
		push_front(list1, new_item);
	}
	
	struct List* test1 = malloc(sizeof(struct List*));
	struct List* test2 = malloc(sizeof(struct List*));
	push_front(test1, 2);
	push_front(test1, 3);
	push_front(test2, 4);
	push_front(test2, 5);
	
	printf("Elements of first list: %d %d\n", test1->head->value, test1->head->next->value);
	printf("Elements of second list: %d %d\n", test2->head->value, test2->head->next->value);
	
	struct List* final_list = merge(test1, test2);
	struct Item* temp = final_list->head;
	int remaining = count_items(final_list);
	
	printf("Merged list: \n");
	for (int i = 1; i <= remaining; i++) {
		printf("%d ", temp->value);
		temp = temp->next;
	}
	printf("\n");
	for (int i = 0; i <= remaining - 1; i++) {
		pop_front(final_list);
	}
	
	for (int i = 0; i <= SIZE - 1; i++) {
		pop_front(list);
		pop_front(list1);
	}
	
	free(list);
	free(list1);
	free(final_list);
	return 0;
}

double check_element(struct List* list, unsigned int index) {
	struct Item* temp = list->head;
	int amount_of_items = count_items(list);
	if (index >= amount_of_items) {
		printf("No such index in the list!\n");
		return 0;
	}
	start = clock();
	for (int i = 0; i < index; i++) {
		temp = temp->next;
	}
	end = clock();
	cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
	return cpu_time_used;
}

int count_items(struct List* list) {
	int counter = 0;
	struct Item* current = list->head;
	while(current != NULL) {
		current = current->next;
		counter++;
	}
	return counter;
}

void push_front(struct List* list, int v) {
	struct Item* new = (struct Item*)malloc(sizeof(struct Item));
	new->value = v;
	//printf("Adding new element to the front of the list: %d\n", new->value);
	new->next = list->head;
	list->head = new;
}

void push_index(struct List* list, int v, unsigned int index) {
	int amount_of_items = count_items(list);
	if (index >= amount_of_items) {
		printf("There is no such index in this list!\n");
		return;
	}
	struct Item* new = (struct Item*)malloc(sizeof(struct Item));
	if (index == 0) {
		push_front(list, v);
	}
	else if (index == amount_of_items - 1) {
		push_back(list, v);
	}
	else {
		struct Item* current;
		struct Item* temp;
		current = list->head;
		new->value = v;
		int i = 0;
		while (i != index - 1) {
			current = current->next;
			i++;
		}
		temp = current->next;
		current->next = new;
		new->next = temp;
		printf("Adding new element to the index %d of the list: %d\n", index, new->value);
	}
}

void push_back(struct List* list, int v) {
	int amount_of_items = count_items(list);
	if (amount_of_items + 1 > SIZE) {
		printf("List is full!\n");
		return;
	}
	struct Item* new = (struct Item*)malloc(sizeof(struct Item));
	new->value = v;
	new->next = NULL;
	printf("Adding new element to the back of the list: %d\n", new->value);
	if (list->head == NULL) {
		list->head = new;
		return;
	}
	struct Item* temp = list->head;
	while (temp->next != NULL) {
		temp = temp->next;
	}
	temp->next = new;
}

void pop_front(struct List* list) {
	if (list->head == NULL) {
		printf("List is empty!\n");
		return;
	}
	//printf("Deleting element from the front of the list: %d\n", list->head->value);
	struct Item* temp = list->head;
	list->head = list->head->next;
	free(temp);
}

void pop_index(struct List* list, unsigned int index) {
	int amount_of_items = count_items(list);
	if (index >= amount_of_items) {
		printf("There is no such index in this list!\n");
		return;
	}
	if (index == 0) {
		pop_front(list);
	}
	else if (index == amount_of_items - 1) {
		pop_back(list);
	}
	else {
		struct Item* current = list->head;
		struct Item* temp;
		int i = 0;
		while (i != index - 1) {
			current = current->next;
			i++;
		}
		temp = current->next;
		current->next = temp->next;
		printf("Deleting an element in the index %d of the list: %d\n", index, temp->value);
		free(temp);
	}
}

void pop_back(struct List* list) {
	if (list->head == NULL) {
		printf("List is empty!\n");
		return;
	}
	if (list->head->next == NULL) {
		printf("Deleting element from the back: %d\n", list->head->value);
		list->head = NULL;
		return;
	}
	struct Item* temp = list->head;
	while (temp->next->next != NULL) {
		temp = temp->next;
	}
	printf("Deleting element from the back: %d\n", temp->next->value);
	free(temp->next);
	temp->next = NULL;
}

struct List* merge(struct List* list1, struct List* list2) {
	//printf("Merging lists!\n");
	struct Item* temp = list1->head;
	while (temp->next != NULL) {
		temp = temp->next;
	}
	temp->next = list2->head;
	return list1;
}

