(* Generate Combinations *)

(*
   [[a,b],[c,d]] -> [[a,c],[a,d],[b,c],[b,d]]
   where the i_th sublist of first list
   is a candidates of i_th element in generated sequences
*)

type lll = int list list [@@deriving show]

let combinations l =
  let prefix es ls =
    List.fold_left (fun acc h -> List.map (fun l -> h :: l) ls @ acc) [] es
  in
  List.fold_left (fun acc h -> prefix h acc) [ [] ] l
;;

let l = [ [ 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7 ] ] in
let r = combinations l in
print_endline (show_lll r);
print_int (List.length r);
print_endline
