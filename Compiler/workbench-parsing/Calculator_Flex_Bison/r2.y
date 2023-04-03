%token INTEGER VARIABLE
%left '+' '-'
%left '*' '/' '%'
%left '^'

%{
    #include <stdio.h>
    #include <math.h>
    int yylex(void);
    void yyerror(char *);
    int sym[26];

%}

/* -----------------

statement_list:
        statement '\n' 
        | statement_list statement '\n';

The above part is to make calculations like "1+2+3+4" to be possible!

one statement is either expr or special "=" expr.

the left side of an expr can be another expr?

----------------- */

%%
statement_list:
        statement '\n' 
        | statement_list statement '\n';

statement:
         expr                   { printf("Out: %d\n", $1); }
         | VARIABLE '=' expr    { sym[$1] = $3; }

expr:
        INTEGER                 { $$ = $1; }
        | VARIABLE              { $$ = sym[$1]; }
        | expr '+' expr         { $$ = $1 + $3; }
        | expr '-' expr         { $$ = $1 - $3; }
        | expr '*' expr         { $$ = $1 * $3; } 
        | expr '/' expr         { $$ = $1 / $3; }
        | expr '%' expr         { $$ = $1 % $3; }
        | expr '^' expr         { $$ = pow($1, $3); }
        | '(' expr ')'          { $$ = $2; }
        ;
%%

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    yyparse();
    return 0;
}

