let () =
  try
    let lex_buf = Lexing.from_channel stdin in
    while true do
      print_string ">>> ";
      flush stdout;
      let result = Parser.starts_parsing Lexer.token lex_buf in
      Printf.printf "%d\n" result;
      flush stdout
    done
  with
  | Lexer.Eof -> exit 0
;;
