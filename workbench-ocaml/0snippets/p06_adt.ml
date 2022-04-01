type set = S of {
    insert : int -> set;
    member : int -> bool;
    size   : unit -> int
}

let empty_set =
  let rec make_set xs =               (* make_set & xs are private *)
    let contains i = List.mem i xs in (* contains is private too *)
    S{ insert =
         (fun i ->
            if contains i then
              make_set xs
            else
              make_set (i::xs));
       member = contains;
       size = (fun () -> List.length xs)}
in make_set [];;

let use_sets () =
  let S s1 = empty_set in
  let S s2 = s1.insert 34 in
  let S s3 = s2.insert 45 in (
    print_int (s3.size ());
    print_endline "");;

let _ = use_sets ();;

(* Note: there is no way to access the data direcetly *)
