CC = gcc
CFLAGS = -Wall -Wextra

ADADIR = ../EX2

all: main

main: main.o library_iterative.o library_recursive.o
	$(CC) $(CFLAGS) -o $@ $^ -lgnat

main.o: main.c
	$(CC) $(CFLAGS) -c $^

library_iterative.o: $(ADADIR)/library_iterative.adb
	gnatmake -c $^

library_recursive.o: $(ADADIR)/library_recursive.adb
	gnatmake -c $^

clean:
	rm -f main *.o *.ali