(* TODO: adding If is just adding a bool type & an extra match *)

type op =
  | Add
  | Mul;;

type expr =
  | Var of string
  | Int of int
  | Node of op * expr * expr
  | Decl of string * expr * expr;;

let is_value : expr -> bool = function
  | Int _ -> true
  | _ -> false;;

let rec sub var value expr =
  match expr with
  | Int _ -> expr
  | Var v -> if var = v then value else expr
  | Node (op,l,r) -> Node (op, sub var value l, sub var value r)
  | Decl (var2, value2, scope) ->
     let value2' = sub var value value2 in
     if var = var2
     then Decl (var2, value2', scope)
     else Decl (var2, value2', sub var value scope);;

(sub "a", (Int 10), Decl("b", Int 5, (Node (Add, Var "a", Var "b"))));;

let rec step (expr:expr) : expr =
  let rec calc operator l r =
    match operator, l, r with
    | Add, Int a, Int b -> Int (a + b)
    | Mul, Int a, Int b -> Int (a * b)
    | _ -> failwith "Op error!" in
  match expr with
  | Int _ -> failwith "Not supposed to happen!"
  | Var _ -> failwith "Unbound var found!"
  | Node (op, l, r) when is_value l && is_value r -> calc op l r
  | Node (op, l, r) when is_value l -> Node (op, l, step r)
  | Node (op, l, r)  -> Node (op, step l, r)
  | Decl (var, value, scope) when is_value value -> sub var value scope
  | Decl (var, value, scope) -> Decl (var, step value, scope)
;;

let rec eval (e:expr):expr =
  if is_value e then e
  else eval(step(e));;


(step (Node(Add,Int 1,Int 2)));;

(step
   (Decl ("a", Int 10,
          (Node (Add, Var "a", Var "a")))));;

(step
   (step
      (step
         (Decl("a", Int 10,
               (Decl("b", Int 5,
                     (Node(Add, Var "a", Var "b")))))))));;

(eval (Decl("a", Int 10,
            Decl("b", Int 5,
                 Node(Add, Var "a", Var "b")))));;

(eval (Decl("a", Int 10,
            Node(Add,Var "a", Int 5))));;


(eval (Decl ("x", Int 10, (Decl("x", Int 5, Var "x")))));;
