%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start starts_parsing
%type <int> starts_parsing
%%

starts_parsing:
    expr EOL { $1 }
;
expr:
    | INT { $1 }
    | LPAREN expr RPAREN { $2 }
    | expr PLUS expr { $1 + $3 }
    | expr MINUS expr { $1 - $3 }
    | expr TIMES expr { $1 * $3 }
    | expr DIV expr { $1 / $3 }
    | MINUS expr %prec UMINUS { - $2 }
;
