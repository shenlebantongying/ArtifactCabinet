%{
#include "r2.tab.h"
#include <stdlib.h>
void yyerror(char *);
%}

%%
    /* integers */

[0-9]+ {
    yylval = atoi(yytext);
    return INTEGER; /* here it generate a token*/
	}

    /* variables */
[a-z] {
	yylval = *yytext ;
	return VARIABLE;
	}

    /* operators */
[-+()=*/%^\n] { return *yytext; }

    /* skip whitespace */
[ \t] {;}

    /* anything else is an error */
. { yyerror("invalid character"); }

%%

int yywrap(void) {
    return 1;
}
