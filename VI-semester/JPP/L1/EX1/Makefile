CC=gcc
CFLAGS=-Wall

all: iterative_main recursive_main

iterative_main: main.c iterative.c
	$(CC) -o iterative_main main.c iterative.c $(CFLAGS)

recursive_main: main.c recursive.c
	$(CC) -o recursive_main main.c recursive.c $(CFLAGS)

clean:
	rm -f iterative_main recursive_main