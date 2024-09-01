(* inclusive range *)
let irange l r = List.init r (fun x -> x + l)

(* p[i] -> price table of length i*)

(* Naive Method1: Brutal force Recursion *)
let rec cut_rod p = function
  | 0 -> 0
  | n ->
    List.fold_left2
      (fun acc i p_i ->
        if 1 <= i && i <= n then max acc (p_i + cut_rod p (n - i)) else acc)
      0
      (irange 0 (List.length p))
      p
;;

let p = [ 0; 1; 5; 8; 9; 10; 17; 17; 20; 24; 30 ] in
List.map (cut_rod p) (irange 0 11)
