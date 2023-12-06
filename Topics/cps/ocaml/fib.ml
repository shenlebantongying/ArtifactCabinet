let fib n =
  let rec aux n k =
    if n <= 1
    then k 1
    else aux (n-1) (fun a1 -> aux (n-2) (fun a2 -> k (a1 + a2)))
  in
  aux n (fun x -> x)
;;

(fib 10)
