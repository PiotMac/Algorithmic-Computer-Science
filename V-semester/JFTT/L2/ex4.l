%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int yywrap();
int yylex();

int* stack;
int pointer = 0;
char received_error[1024] = "OK";

void restart() {
    free(stack);
    stack = malloc(sizeof(int) * 1024);
    pointer = 0;
    strcpy(received_error, "OK");
}

void print_result() {
    if (strcmp(received_error, "OK") != 0) {
        printf("%s\n", received_error);
    }
    else if (strcmp(received_error, "OK") == 1 && pointer > 1) {
        printf("Error: Not enough operators!\n");
    }
    else {
        printf("= %d\n\n", stack[0]);
    }

    restart();
}

int is_possible_operation() {
    return (pointer >= 2); // Check if there are at least two operands on the stack
}

void perform_operation(int result) {
    stack[pointer - 2] = result;
    pointer--;
}

void add() {
    if (is_possible_operation()) {
        int result = stack[pointer - 2] + stack[pointer - 1];
        perform_operation(result);
    } else {
        strcpy(received_error, "Error: Not enough arguments for addition!\n");
    }
}

void subtract() {
    if (is_possible_operation()) {
        int result = stack[pointer - 2] - stack[pointer - 1];
        perform_operation(result);
    } else {
        strcpy(received_error, "Error: Not enough arguments for substraction!\n");
    }
}

void multiply() {
    if (is_possible_operation()) {
        int result = stack[pointer - 2] * stack[pointer - 1];
        perform_operation(result);
    } else {
        strcpy(received_error, "Error: Not enough arguments for multiplication!\n");
    }
}

void divide() {
    if (!is_possible_operation()) {
        strcpy(received_error, "Error: Not enough arguments for division!\n");
    }
    else if (stack[pointer - 1] == 0) {
        pointer--;
        strcpy(received_error, "Error: Division by zero!\n");
    }
    else {
        int result = stack[pointer - 2] / stack[pointer - 1];
        perform_operation(result);
    }
}


void power() {
    if (is_possible_operation()) {
        int result = pow((double)stack[pointer - 2], (double)stack[pointer - 1]);
        perform_operation(result);
    } else {
        strcpy(received_error, "Error: Not enough arguments for exponentiation!\n");
    }
}

void modulo() {
    if (!is_possible_operation()) {
        strcpy(received_error, "Error: Not enough arguments for modulo!\n");
    }
    else if (stack[pointer - 1] == 0) {
        pointer--;
        strcpy(received_error, "Error: Modulo by zero!\n");
    }
    else  {
        int result = stack[pointer - 2] % stack[pointer - 1];
        perform_operation(result);
    }
}

void wrong_symbol(char symbol) {
    strcpy(received_error, "Error: Unexpected symbol encountered: '");
    char symbol_str[2] = {symbol, '\0'};  // Convert the symbol to a string
    strcat(received_error, symbol_str);
    strcat(received_error, "'\n");
}

void exit_program() {
    printf("Exiting the program.\n");
    exit(0);
}

%}

%%
[^ \t0-9\+\-\*\/\^\%\n]           { wrong_symbol(yytext[0]); }

\-?[0-9]+                         { stack[pointer] = atoi(yytext); pointer++; }
[ \t]+                            {}

\+                                { add(); }
\-                                { subtract(); }
\*                                { multiply(); }
\/                                { divide(); }
\^                                { power(); }
\%                                { modulo(); }

\n                                { print_result(); }

^"exit"$                          { exit_program(); }
%%

int main() {
    stack = malloc(sizeof(int) * 1024);
    return yylex();
}
