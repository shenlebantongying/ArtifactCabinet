(*
https://cs3110.github.io/textbook/cover.html
*)
(* All parts are supposed to run in interactive way *)



(* reverse a string *)

let revStr s =
  let len = String.length s in
  let reversed = Bytes.create len in
  let ss= Bytes.unsafe_of_string s in
  for i = 0 to len - 1 do
    Bytes.unsafe_set reversed (len - 1 - i) (Bytes.unsafe_get ss i)
  done;
  Bytes.unsafe_to_string reversed;;

(revStr "abcde") = "edcba";;

(* reverse a list *)
let revlst lst=
  let rec aux acc = function    (* Note-> function will make func take extra atg*)
    | [] -> acc
    | h::t -> aux (h::acc) t
  in aux [] lst;;

(revlst [1;2;3]) = [3;2;1];;


(* Slow fib *)
let rec fib = function
  | 1 -> 1
  | 2 -> 1
  | n -> fib (n - 1) + fib (n - 2);;

(fib 10)

(* Fast  *)
let rec h ni pp p =
    if ni = 1 then p
    else h (ni-1) p (pp+p);;

let fastfib n = h n 0 1;;

(fastfib 10)
(*

h 10 0 1
h 9 1 1
h 8 1 2
h 7 2 3
h 6 3 5
h 5 5 8
h 4 8 13
...
*)



(* 3. Data and Types *)
(* simple construction *)

let rec mydownto = function
  | 0 -> []
  | x -> x::mydownto(x-1)
;;

mydownto 10;;

let upto n =
  let rec aux o =
    if o = n then [] else o::(aux (o+1))
  in aux 1;;

upto 10;;


let rec concate = function
  | [] -> ""
  | hd::tl -> hd ^ concate tl;;

concate ["nice ";"ok";" sure"];;



(* 4. Correctness and efficiency *)


