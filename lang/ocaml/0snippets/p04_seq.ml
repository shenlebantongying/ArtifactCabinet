open List;;

(* Played on OCaml 4.14 *)
(* seq is kind of lazy list *)

type 'a seq =
  | Nil
  | Cons of 'a * (unit -> 'a seq);;

let head (Cons (x, _)) = x;;
let tail (Cons (_,xf)) = xf ();;

let rec from k = Cons (k, fun() -> from (k+1));;

let it = from 1;;

head (tail (tail it));;

let rec get n s=
  match n, s with
  | 0, _ -> []
  | _, Nil -> []
  | n, Cons (x, xf) -> x :: get (n-1) (xf ());;


get 10 (from 60);;

(* TODO: read Lecture 9 from Madhavapeddy's OCaml course *)
