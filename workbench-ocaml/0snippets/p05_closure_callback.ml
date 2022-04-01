let cbs : (int -> unit) list ref = ref [];;

let onKeyEvent f = cbs := f::(!cbs);;

let onEvent i =
  let rec loop fs =
    match fs with
    | [] -> ()
    | f::fs' -> (f i; loop fs')
  in loop (!cbs);;

let timesPressed = ref 0;;
let _ = onKeyEvent (fun _ -> timesPressed := (!timesPressed) + 1);;

let print_if_pressed i =
  onKeyEvent (fun j -> if i=j
               then print_endline ("pressed " ^ string_of_int i)
               else ());;

let _ = print_if_pressed 3;;
let _ = print_if_pressed 4;;
let _ = print_if_pressed 5;;

(* usage *)

onEvent 3;;
