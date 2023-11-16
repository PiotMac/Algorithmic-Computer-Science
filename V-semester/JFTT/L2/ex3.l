%{
#include <stdio.h>

int doxygen_enabled = 0;  // Flag to track whether Doxygen mode is enabled
%}

new_line_break (\\(\r)*\n)
single_line_comment (\/{new_line_break}*\/)
multi_line_comment (\/{new_line_break}*\*)
end_multi_line_comment (\*{new_line_break}*\/)
single_quote (\')
double_quotes (\")
include ^\#include[ ]*<

%x IN_SINGLE_QUOTE_STRING
%x IN_DOUBLE_QUOTES_STRING
%x SINGLE_LINE_COMMENT
%x MULTI_LINE_COMMENT
%x SINGLE_LINE_DOXYGEN_COMMENT
%x MULTI_LINE_DOXYGEN_COMMENT
%x INCLUDE

%%

{include}                                       { BEGIN(INCLUDE); ECHO; }
<INCLUDE>>                                      { BEGIN(INITIAL); ECHO; }
<INCLUDE>.                                      { ECHO; }

{single_line_comment}{new_line_break}*\/        { BEGIN(SINGLE_LINE_DOXYGEN_COMMENT); if(doxygen_enabled) ECHO; }
{single_line_comment}{new_line_break}*!         { BEGIN(SINGLE_LINE_DOXYGEN_COMMENT); if(doxygen_enabled) ECHO; }

{multi_line_comment}{new_line_break}*\*         { BEGIN(MULTI_LINE_DOXYGEN_COMMENT); if(doxygen_enabled) ECHO; }
{multi_line_comment}{new_line_break}*!          { BEGIN(MULTI_LINE_DOXYGEN_COMMENT); if(doxygen_enabled) ECHO; }

{single_line_comment}                           { BEGIN(SINGLE_LINE_COMMENT); }
{multi_line_comment}                            { BEGIN(MULTI_LINE_COMMENT); }

{single_quote}                                  { BEGIN(IN_SINGLE_QUOTE_STRING); ECHO; }
{double_quotes}                                 { BEGIN(IN_DOUBLE_QUOTES_STRING); ECHO; }

<IN_SINGLE_QUOTE_STRING>{single_quote}          { BEGIN(INITIAL); ECHO; }
<IN_DOUBLE_QUOTES_STRING>{double_quotes}        { BEGIN(INITIAL); ECHO; }

<IN_SINGLE_QUOTE_STRING>.                       { ECHO; }
<IN_SINGLE_QUOTE_STRING>\n                      { ECHO; }

<IN_DOUBLE_QUOTES_STRING>.                      { ECHO; }
<IN_DOUBLE_QUOTES_STRING>\n                     { ECHO; }

<SINGLE_LINE_COMMENT>{new_line_break}           {}
<SINGLE_LINE_COMMENT>\n                         { BEGIN(INITIAL);}
<SINGLE_LINE_COMMENT>.                          {}

<MULTI_LINE_COMMENT>.                           {}
<MULTI_LINE_COMMENT>\n                          {}

<SINGLE_LINE_DOXYGEN_COMMENT>.                  { if(doxygen_enabled) ECHO; }
<SINGLE_LINE_DOXYGEN_COMMENT>{new_line_break}   {}
<SINGLE_LINE_DOXYGEN_COMMENT>\n                 { BEGIN(INITIAL); if(doxygen_enabled) ECHO; }

<MULTI_LINE_DOXYGEN_COMMENT>.                   { if(doxygen_enabled) ECHO; }
<MULTI_LINE_DOXYGEN_COMMENT>\n                  { if(doxygen_enabled) ECHO; }

<MULTI_LINE_COMMENT>"*/"                        { BEGIN(INITIAL); }
<MULTI_LINE_DOXYGEN_COMMENT>"*/"                { BEGIN(INITIAL); if(doxygen_enabled) ECHO; }

.                                               { ECHO; }
\n                                              { ECHO; }

%%

int main(int argc, char* argv[]) {
    if (argc < 2 || argc > 3) {
        fprintf(stderr, "Usage: %s [-dox] input_file.cpp\n", argv[0]);
        return 1;
    }
    yyin;

    if (argc == 2) {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL) {
            perror("Error opening input file");
            return 1;
        }
    }

    if (argc == 3 && strcmp(argv[1], "-dox") == 0) {
        doxygen_enabled = 1;
        yyin = fopen(argv[2], "r");
        if (yyin == NULL) {
            perror("Error opening input file");
            return 1;
        }
    }
    else if (argc == 3) {
        fprintf(stderr, "Wrong flag used! Use '-dox'\n");
        return 1;
    }

    yylex();

    fclose(yyin);;

    return 0;
}
