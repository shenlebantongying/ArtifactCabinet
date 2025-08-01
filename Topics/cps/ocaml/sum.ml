let sum s =
  let rec sum' s k=
    match s with
      [] -> k 0
    | x::xs -> sum' xs (fun a -> k (x+a)) in
  sum' s (fun x -> x);;

sum [1;2;3;4];;

(*

  * -> apply

  sum x1, x2, x3;;

  will be expanded into

  (fun x -> x) * (fun a -> x1 + a) * (fun a -> x2 + a) * (fun a -> x3 + a) * 0

 *)
