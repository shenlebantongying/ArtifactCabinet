(* Some interactions between list & variants*)

type myT = One of string | Two of string

let myData = [ One "ok"; Two "what" ]

let unwrap = function
  | One str -> "Type 1 -> " ^ str
  | Two str -> "Type 2 -> " ^ str

let rec myRec str_lst =
  match str_lst with
  | [] -> None
  | h :: t ->
      print_endline (unwrap h);
      myRec t

let rev str_lst =
  let rec aux acc lst =
    match lst with [] -> acc | h :: t -> aux (h :: acc) t
  in
  aux [] str_lst
;;

myRec myData

(* List.iter (Printf.printf "%s ") (rev myData);; *)
