(* Calculating n permutations*)

(* Method 1 -> The insert-to-all-positions / brutal force
   {v
   Base case ->  [] -> no combination
   One       -> [a] -> [a]
   Two       -> [a,b] -> [a,b] [b,a]
   three     -> [a,b,c] ->
      Base on the previous case -> c can be in many places
         insert c into [a,b] -> [c,a,b] [a,c,b] [a,b,c]
         insert c into [b,a] -> [c,b,a] [b,c,a] [b,a,c]
   v}
*)

type ll = int list [@@deriving show]
type lll = ll list [@@deriving show]

let rec permutate l =
  (*
     generate all combinations when inserting a single element, like
     {v
         rest (or the original list)
         v---v
         o o o x
         o o x o
         o x o o
         x o o o
            ^---^ the prev list which was positioned before
     v}
  *)
  let rec gen_combinations e (prev : int list) (rest : int list) =
    match rest with
    | [] -> [ e :: prev ]
    | h :: t -> ((h :: t) @ (e :: prev)) :: gen_combinations e (h :: prev) t
  in
  match l with
  | [] -> [ [] ]
  | h :: t -> List.concat_map (gen_combinations h []) (permutate t)
;;

print_endline (show_lll (permutate [ 1; 2; 3 ]))

let rec dup_exist = function
  | [] -> "ok"
  | hd :: tl -> if List.exists (( = ) hd) tl then show_ll hd else dup_exist tl
;;

print_endline (dup_exist (permutate [ 1; 2; 3; 4; 5 ]))
