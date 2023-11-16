(* aka double-ended queue *)

(* NOTE: for efficient imple
   https://github.com/ocaml-batteries-team/batteries-included/blob/master/src/batDeque.ml *)

module Dq = struct
  type 'a queue =
    { f : 'a list (* front *)
    ; b : 'a list (* back *)
    }

  let empty = { f = []; b = [] }

  (* NOTE: reading & writing to head and tail are efficient *)

  let cons x q = { f = x :: q.f; b = q.b }
  let snoc x q = { f = q.f; b = x :: q.b }

  let head q =
    match q with
    | { f = []; b = [] } -> None
    | { f = []; b = bh :: _ } -> Some bh
    | { f = x :: _; _ } -> Some x
  ;;

  let tail q =
    match q with
    | { f = []; b = [] } -> None
    | { f; b = [] } -> List.nth_opt f (List.length f - 1)
    | { f = _; b = x :: _ } -> Some x
  ;;

  let splitList (l : 'a list) =
    let len = List.length l in
    let mid = len / 2 in
    let rec aux n acc (l : 'a list) : 'a list * 'a list =
      match l, acc with
      | hd :: tl, (a1, a2) ->
        if n < mid
        then aux (n + 1) (hd :: a1, a2) tl
        else aux (n + 1) (a1, hd :: a2) tl
      | [], acc -> acc
    in
    aux 0 ([], []) l
  ;;

  let toList q = q.f @ List.rev q.b

  let fromList (l : 'a list) : 'a queue =
    let l1, l2 = splitList l in
    { f = List.rev l1; b = l2 }
  ;;

  (* Destructive *)

  let pop_head q : 'a queue =
    match q with
    | { f = []; b } -> fromList (List.tl (List.rev b))
    | { f = _ :: ft; b } -> { f = ft; b }
  ;;

  let pop_tail q =
    match q with
    | { f; b = _ :: bt } -> { f; b = bt }
    | { f; b = [] } -> fromList (List.tl f)
  ;;
end

let sampleQ =
  Dq.cons 1 @@ Dq.cons 2 @@ Dq.cons 3 @@ Dq.empty
  |> Dq.snoc 4
  |> Dq.snoc 5
  |> Dq.snoc 6
;;

Dq.toList sampleQ;;
Dq.tail sampleQ;;
Dq.head sampleQ;;
Dq.pop_head sampleQ;;
Dq.pop_head (Dq.pop_head sampleQ);;
Dq.pop_head (Dq.pop_head (Dq.pop_head sampleQ));;
Dq.toList @@ Dq.pop_head @@ Dq.pop_head @@ Dq.pop_head @@ Dq.pop_head sampleQ;;

sampleQ
|> Dq.pop_head
|> Dq.pop_head
|> Dq.pop_head
|> Dq.pop_head
|> Dq.pop_head
|> Dq.toList
;;

Dq.toList @@ Dq.pop_tail @@ Dq.pop_tail @@ sampleQ
