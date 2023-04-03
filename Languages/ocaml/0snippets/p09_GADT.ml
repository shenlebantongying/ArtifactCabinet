(* https://ocaml.org/manual/gadts-tutorial.html *)

(* _ below means anonymous type *)
type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term;;

let rec eval : type a. a term -> a = function
  | Int n -> n                  (* a = int *)
  | Add -> (fun x y -> x+y)     (* a = int -> int -> int*)
  | App(f,x) -> (eval f) (eval x);;

eval (App (App (Add, Int 1), (Int 1)));;
