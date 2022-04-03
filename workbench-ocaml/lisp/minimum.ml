exception Syntax_error of string
exception Runtime_error of string

type expr =
  | Nil
  | String of string
  | Inte of Int.t
  | List of expr list
  | Fun of (expr list -> expr);;

let plus a b = Inte (a + b);;

let eval = function
  | Inte a -> Inte a
  | List [Inte a; Inte b]-> plus a b
  | _ -> Nil;;

eval (List [Inte 1; Inte 3]);;
