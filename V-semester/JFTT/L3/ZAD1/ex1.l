%option noyywrap
%{
#include <stdio.h>
#include <stdlib.h>

#include "ex1_y.h"

#define GF 1234577

int yylex();
%}

%%
^\#(.|\\\n)*\n                    {/* Zignoruj komentarz wraz z możliwym break linem*/}
\\\n                              {/* Zignoruj break line */}


\+                                { return ADD; }
\-                                { return SUB; }
\*                                { return MUL; }
\/                                { return DIV; }
\^                                { return POW; }
\%                                { return MOD; }
\(                                { return LPAR; }
\)                                { return RPAR; }

[0-9]+                            { int value; sscanf(yytext, "%d", &value); yylval = value; return NUM; }

[ \t]+                            {/* Zignoruj białe znaki */}

\n                                { return RESULT; }

^"exit"$                          { printf("Exiting the program.\n"); return 0; }

.                                 { return ERR; }
%%