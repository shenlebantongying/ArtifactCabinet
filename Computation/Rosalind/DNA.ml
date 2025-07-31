module Char_tbl = Hashtbl.Make (struct
    type t = char

    let equal = Char.equal
    let hash = Hashtbl.hash
  end)

let () =
  let data =
    In_channel.with_open_text "./data/rosalind_dna.txt" In_channel.input_all
    |> String.trim
    |> String.to_seq
  in
  let rec process (acc : int Char_tbl.t) = function
    | Seq.Nil -> ()
    | Seq.Cons (h, t) ->
      Char_tbl.replace acc h (Char_tbl.find acc h + 1);
      process acc (t ())
  in
  let counter = Char_tbl.create 5 in
  Char_tbl.add counter 'G' 0;
  Char_tbl.add counter 'T' 0;
  Char_tbl.add counter 'C' 0;
  Char_tbl.add counter 'A' 0;
  process counter (data ());
  Array.iter
    (fun c -> Printf.printf "%d " (Char_tbl.find counter c))
    [| 'A'; 'C'; 'G'; 'T' |]
;;
