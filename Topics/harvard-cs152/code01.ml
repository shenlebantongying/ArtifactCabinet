type exp =
  | EVar of string
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp
;;

let rec eval e env =
  match e with
  | EVar x -> (env x)
  | EInt n -> n
  | EAdd (e1,e2) -> (eval e1 env) + (eval e2 env)
  | EMul (e1,e2) -> (eval e1 env) * (eval e2 env)
;;

eval (EInt 1) (fun x -> 0);;

(* Usage of env -> to replace some string to int,
   If there are no use of env, just return 0 as invalid.*)

eval (EVar "x") (fun x -> if x == "x" then 1 else 2)
;;

eval (EAdd (EInt 2, EInt 1)) (fun x -> 0);;
;;

(* 2*3+1 = (2*3)+1 vs 2*(3+1) *)
eval (EAdd (EMul (EInt 2, EInt 3), EInt 1)) (fun x -> 0)
;;

eval (EMul (EInt 2, EAdd (EInt 3, EInt 1))) (fun x -> 0)
;;
