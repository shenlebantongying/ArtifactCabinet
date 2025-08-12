#require "base"

open Base

let rec merge x y =
  (* Conquer *)
  match x, y with
  | [], r -> r
  | l, [] -> l
  | hx :: tx, hy :: ty ->
    if hx < hy
    then hx :: merge tx (hy :: ty) (* Pick the smaller one between two heads*)
    else hy :: merge ty (hx :: tx)
;;

(* Put it in front then let the rest keep comparing *)

let rec merge_sort l =
  match l with
  | [] -> []
  | [ x ] -> [ x ] (* 1 or 0 elements no need to sort *)
  | __ ->
    let half_length = List.length l / 2 in
    let left = List.take l half_length in
    let right = List.drop l half_length in
    merge (merge_sort left) (merge_sort right)
;;

(* Dividing *)

merge_sort [ 6; 4; 5; 7; 2; 5; 3; 4 ]

(*
   [6; 4; 5; 7; 2; 5; 3; 4]
   [6; 4; 5; 7][2; 5; 3; 4]
   [6; 4][5; 7][2; 5][3; 4]
   [6][4][5][7][2][5][3][4]
   [4; 6][5; 7][2; 5][3; 4]
   [4; 5; 6; 7][2; 3; 4; 5]
   [2; 3; 4; 4; 5; 5; 6; 7]
*)
