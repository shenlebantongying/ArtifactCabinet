{
exception Eof
}
rule token = parse
    [' ' '\t']       { token lexbuf }
    | ['\n' ]        { Parser.EOL }
    | ['0'-'9']+ as some_number { Parser.INT(int_of_string some_number) }
    | '+'            { Parser.PLUS }
    | '-'            { Parser.MINUS }
    | '*'            { Parser.TIMES }
    | '/'            { Parser.DIV }
    | '('            { Parser.LPAREN }
    | ')'            { Parser.RPAREN }
    | eof            { raise Eof }