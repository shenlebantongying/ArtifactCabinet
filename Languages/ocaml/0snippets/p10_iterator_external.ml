(* https://xavierleroy.org/control-structures/book/main007.html *)

(* external iterator *)
let arr_iter (l : 'a list) : unit -> 'a option =
  let i = ref 0 in
  fun () ->
    if !i >= List.length l
    then None
    else (
      let res = List.nth l !i in
      incr i;
      Some res)
;;

let rec iter_f_acc f acc it =
  match it () with
  | None -> acc
  | Some v -> iter_f_acc f (f acc v) it
;;

iter_f_acc ( + ) 0 (arr_iter [ 1; 2; 3; 4; 5 ])

(* Next func *)

exception StopIteration

let next (iter : unit -> 'a option) : 'a =
  match iter () with
  | None -> raise StopIteration (* ? *)
  | Some v -> v
;;

let a = arr_iter [ 1; 2; 3; 4; 5 ] in
let loop =
  while true do
    let v = next a in
    print_int v;
    print_endline ""
  done
in
try loop with
| StopIteration -> ()
| _ -> ()
