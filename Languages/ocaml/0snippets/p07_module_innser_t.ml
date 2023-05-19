module Hybird = struct
  type t =                      (* Note that the Nil/String  *)
    | Nil
    | String of string
  let to_str = function
    | Nil -> "nil"
    | String s -> s
  let pprint s = ( print_endline (to_str s))
end

let x = Hybird.String "asd";;
Hybird.pprint x;;
