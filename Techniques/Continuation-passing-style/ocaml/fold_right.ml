let fold_right (op : 'a -> 'b -> 'b) (lst: 'a list) (init: 'b) : 'b =
  let rec fold_right' lst k =
    match lst with
    | [] -> k init
    | x::xs -> fold_right' xs (fun a -> k (op x a)) in
  fold_right' lst (fun x -> x);;

fold_right (fun x y -> x + y) [1;2;3] 0;;

(*

  fold_right op [x1;x2] init;;

  * -> apply

  (fun x -> x) * (fun a -> (op x1 a)) * (fun a -> (op x2 a)) * init

 *)

(fun a -> (+) 1 a) @@ (fun a -> (+) 2 a ) @@ (fun a -> (+) 3 a) 0;;
