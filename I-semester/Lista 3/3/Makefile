# Makefile

all: main

main: main.o phi.o pierwsze.o
	clang -o main main.o phi.o pierwsze.o
	
main.o: main.c
	clang -c main.c
	
phi.o: phi.c
	clang -c phi.c
	
pierwsze.o: pierwsze.c
	clang -c pierwsze.c
	
clean:
	rm -phi pierwsze main *.o *~
