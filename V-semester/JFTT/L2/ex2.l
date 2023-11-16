%{
#include <stdio.h>

int in_comment = 0;
%}

enter                (<!--)
exit                 (-->)

enter_CDATA          (<!\[CDATA\[)
exit_CDATA           (\]\]>)

single_quote         (\')
double_quotes        (\")


%x BROKEN
%x COMMENT
%x CDATA
%x IN_SINGLE_QUOTE_STRING
%x IN_DOUBLE_QUOTES_STRING

%%
{enter}                                         { BEGIN(COMMENT); }
<COMMENT>{exit}                                 { BEGIN(INITIAL); }
<COMMENT>--                                     { BEGIN(BROKEN); printf("ERROR! '--' IN THE COMMENT DETECTED!\n"); }
<COMMENT>.                                      {}
<COMMENT>\n                                     {}
<BROKEN>.                                       { ECHO; }
<BROKEN>\n                                      { ECHO; }

{single_quote}                                  { BEGIN(IN_SINGLE_QUOTE_STRING); ECHO; }
{double_quotes}                                 { BEGIN(IN_DOUBLE_QUOTES_STRING); ECHO; }

<IN_SINGLE_QUOTE_STRING>{single_quote}          { BEGIN(INITIAL); ECHO; }
<IN_DOUBLE_QUOTES_STRING>{double_quotes}        { BEGIN(INITIAL); ECHO; }

<IN_SINGLE_QUOTE_STRING>.                       { ECHO; }
<IN_SINGLE_QUOTE_STRING>\n                      { ECHO; }

{enter_CDATA}                                   { BEGIN(CDATA); ECHO; }
<CDATA>{exit_CDATA}                             { BEGIN(INITIAL); ECHO; }
<CDATA>.                                        { ECHO; }
<CDATA>\n                                       { ECHO; }

.                                               { ECHO; }
\n                                              { ECHO; }
%%

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s input_file.xml\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
        perror("Error opening input file");
        return 1;
    }

    yylex();

    fclose(yyin);

    return 0;
}
