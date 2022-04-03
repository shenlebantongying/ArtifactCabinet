(* my_stack *)

(* stack *)

(* to_list for stack *)
let to_list s =
  let l = ref [] in
  Stack.iter (fun x -> l := x::!l) s;
  !l;;

let pprint_list l =
  List.iter (Printf.printf "%d ") l;print_endline "";;


let a_stck = Stack.create () in
Stack.push 1 a_stck; pprint_list (to_list a_stck);
Stack.push 2 a_stck; pprint_list (to_list a_stck);
Stack.push 2 a_stck; pprint_list (to_list a_stck);
ignore (Stack.pop a_stck); pprint_list (to_list a_stck);
ignore (Stack.pop a_stck); pprint_list (to_list a_stck);
;
