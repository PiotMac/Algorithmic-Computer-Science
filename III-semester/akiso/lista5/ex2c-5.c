#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <stdlib.h>

int count = 1;

void mysignal() {
	printf("Received %d signal!\n", count);
	count++;
}

int main() {
	int status;
	signal(10, mysignal);
	pid_t k;
	k = fork();
	//If k is a child
	if (k == 0) {
		for (int i = 1; i <= 10000; i++) {
			kill(getppid(), 10);
			printf("Sent signal %d\n", i);
		}
		exit(1);
	}
	//If k is a parent
	else {
		wait(&status);
	}
}
