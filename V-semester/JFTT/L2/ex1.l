%{
#include <stdio.h>

int lines = 1;
int words = 0;
FILE *output_file;
%}

%%
^[ \t]*\n           { /* likwidowanie pustych linii */ }
^[ \t]+             { /* usuwanie białych znaków na początku wiersza */ }
[ \t]+$             { /* usuwanie białych znaków na końcu wiersza */ }
[ \t]+              { fprintf(output_file, " "); } /* zamiana ciągów tabulatorów i spacji na jedną spację */
([^ \t\n])+/[ \t]   { words++; fprintf(output_file, "%s", yytext); }
([^ \t\n])+$        { words++; fprintf(output_file, "%s", yytext); }
\n                  { lines++; fprintf(output_file, "\n");} /* zliczanie linii */

%%

int main(int argc, char* argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s input_file.txt output_file.txt\n", argv[0]);
        return 1;
    }

    output_file = fopen(argv[2], "w");
    if (output_file == NULL) {
        perror("Error opening output file");
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
        perror("Error opening input file");
        fclose(output_file);
        return 1;
    }

    if (feof(yyin)) {
        printf("The file is empty.\n");
        return 1;
    }

    yylex();

    fclose(yyin);
    fclose(output_file);

    if (words == 0) {
        printf("Liczba linii: 0\nLiczba słów: 0\n");
    }
    else {
        printf("Liczba linii: %d\nLiczba słów: %d\n", lines, words);
    }
    return 0;
}
