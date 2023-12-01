%{
  #include <stdio.h>
  #include <math.h>
  #include <string.h>
  #include <stdlib.h>
  #include <stdbool.h>
  #define GF 1234577
  #define GFpow 1234576
  #define MAX_LENGTH 2048

  int yylex();
  void yyerror (char*);
  char rev_polish_notation[MAX_LENGTH];
  char error_message[MAX_LENGTH];
  bool is_error = false;

  void appendRPN(char* token) {
    strcat(rev_polish_notation, token);
    strcat(rev_polish_notation, " ");
  }

  void reset() {
    error_message[0] = '\0';
    rev_polish_notation[0] = '\0';
  }

  int add(int a, int b, int m) {
    return (a + b) % m;
  }

  int subtract(int a, int b, int m) {
    int result = (a - b) % m;
    if (result < 0) {
        result += m;
    }
    return result % m;
  }

  int multiply(int a, int b, int m) {
    int first = a % m;
    int second = b % m;
    int result = first; 
    for(int i = 1; i < second; i++) {
        result += first;
        result = ((result % m) + m) % m;
    }
    return result;
  }

  int extended_gcd(int a, int b, int *x, int *y) {
    if (a == 0) {
        *x = 0;
        *y = 1;
        return b;
    }

    int x1, y1;
    int gcd = extended_gcd(b % a, a, &x1, &y1);

    *x = y1 - (b / a) * x1;
    *y = x1;

    return gcd;
}

int mod_invert(int a, int m) {
    int x, y;
    int gcd = extended_gcd(a, m, &x, &y);

    if (gcd != 1) {
        // Modular inverse does not exist
        //printf("Nie istnieje liczba odwrotna.\n");
        return -1;
    } else {
        int inverse = (x % m + m) % m;
        return inverse;
    }
}

int invert(int num, int m) {
    return mod_invert(num, m);
}


  int divide(int a, int b, int m) {
    int inv = invert(b, m);
    if (inv == -1) {
      return -1;
    }
    inv = inv % m;
    return (a * inv) % m;
    //return multiply(a, inv, m) % m; 
  }

  int power(long int a, int b, int m) {
    int result = 1;

    for(int i = 1; i <= b; i++) {
        result = ((((result % m) + m) % m) * (((a % m) + m) % m)) % m;
        result = ((result % m) + m) % m;
    }
    return result;
  }
  
  int modulo(int a, int b, int m) {
    return (a % b + m) % m;
  }

%}

%define api.value.type {int}

%token NUM
%token ADD
%token SUB
%token MUL
%token DIV
%token POW
%token MOD
%token LPAR
%token RPAR
%token RESULT
%token ERR

%left ADD SUB
%left MUL DIV MOD
%precedence NEG
%nonassoc POW

%%
input:
  %empty
| input line RESULT              {is_error = false;}
;

line:
  exp                            { printf("%s\n", rev_polish_notation); if (!is_error) { printf("Wynik:    %d\n", $1); is_error = false; }; reset(); }
| exp ERR           
| ERR               
| exp error
| error
;

exp:
  NUM                            { $$ = $1 % GF; sprintf(rev_polish_notation + strlen(rev_polish_notation), "%d ", $$);}
| SUB NUM %prec NEG              { $$ = subtract(0, $2, GF); sprintf(rev_polish_notation + strlen(rev_polish_notation), "%d ", $$); }
| exp ADD exp                    { appendRPN("+"); $$ = add($1, $3, GF); }
| exp SUB exp                    { appendRPN("-"); $$ = subtract($1, $3, GF); }
| exp MUL exp                    { appendRPN("*"); $$ = multiply($1, $3, GF); }
| exp DIV exp                    { appendRPN("/"); if($3 == 0) {
                                                     is_error = true; 
                                                     sprintf(error_message, "Dzielenie przez 0"); 
                                                     yyerror(error_message); 
                                                   }
                                                   else {
                                                     int result = divide($1, $3, GF);
                                                     if (result == -1) { 
                                                       is_error = true;
                                                       sprintf(error_message, "%d nie jest odwracalne modulo %d\n", $3, GF); 
                                                       yyerror(error_message); 
                                                     }
                                                     else {
                                                       $$ = result;
                                                     }
                                                   }
                                  }
| exp POW exp1                   { $$ = power($1, $3, GF); if (!is_error) { appendRPN("^"); } else { reset(); }}
| exp MOD exp                    { appendRPN("%"); if($3 == 0) { is_error = true; sprintf(error_message, "Modulo przez 0"); yyerror(error_message); } else { $$ = modulo($1, $3, GF); } }
| LPAR exp RPAR                  { $$ = $2; }
| SUB LPAR exp RPAR %prec NEG    { appendRPN("~"); $$ = ((-$3 % GF) + GF) % GF; }
;

exp1:
  NUM                              { $$ = $1 % GFpow; sprintf(rev_polish_notation + strlen(rev_polish_notation), "%d ", $$);}
| SUB NUM %prec NEG                { $$ = subtract(0, $2, GFpow); sprintf(rev_polish_notation + strlen(rev_polish_notation), "%d ", $$); }
| exp1 ADD exp1                    { appendRPN("+"); $$ = add($1, $3, GFpow); }
| exp1 SUB exp1                    { appendRPN("-"); $$ = subtract($1, $3, GFpow); }
| exp1 MUL exp1                    { appendRPN("*"); $$ = multiply($1, $3, GFpow); }
| exp1 DIV exp1                    { appendRPN("/"); if($3 == 0) {
                                                     is_error = true; 
                                                     sprintf(error_message, "Dzielenie przez 0"); 
                                                     yyerror(error_message); 
                                                   }
                                                   else {
                                                     int result = divide($1, $3, GFpow);
                                                     if (result == -1) { 
                                                       is_error = true;
                                                       sprintf(error_message, "%d nie jest odwracalne modulo %d\n", $3, GFpow); 
                                                       yyerror(error_message); 
                                                     }
                                                     else {
                                                       $$ = result;
                                                     }
                                                   }
                                  }
| exp1 MOD exp1                    { appendRPN("%"); if($3 == 0) { is_error = true; sprintf(error_message, "Modulo przez 0"); yyerror(error_message); } else { $$ = modulo($1, $3, GFpow); } }
| LPAR exp1 RPAR                  { $$ = $2; }
| SUB LPAR exp1 RPAR %prec NEG    { appendRPN("~"); $$ = ((-$3 % GFpow) + GFpow) % GFpow; }
;
%%


void yyerror(char *s)
{
    if (error_message[0] == '\0') {
        printf("Błąd: zła składnia!\n");
    } else {
        printf("Błąd: %s\n", s);
    }
    reset();
}

int main()
{
    yyparse();
    return 0;
}