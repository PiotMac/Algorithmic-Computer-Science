# Compiler
CC = gcc

# Compiler flags
CFLAGS = -Wall -Wextra -Werror -g

# Libraries
LIBRECUR = ../EX3/recursive
LIBITER = ../EX3/iterative


# Default target
all: recur iter

recur: main.c recursive
	$(CC) $(CFLAGS) -o $@ main.c -L$(LIBRECUR)/target/release -lrecursive -lpthread -ldl

iter: main.c iterative
	$(CC) $(CFLAGS) -o $@ main.c -L$(LIBITER)/target/release -literative -lpthread -ldl

iterative:
	cd ../EX3/iterative && cargo build --release && cd ../../EX7/

recursive:
	cd ../EX3/recursive && cargo build --release && cd ../../EX7/

clean:
	rm -rf recur iter $(LIBRECUR)/taget $(LIBITER)/target $(LIBRECUR)/Cargo.lock $(LIBITER)/Cargo.lock