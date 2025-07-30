type nucleotide =
  | A
  | C
  | G
  | T

let dna_of_string s =
  let f = function
    | 'A' -> A
    | 'C' -> C
    | 'G' -> G
    | 'T' -> T
    | _ -> failwith "Big news! New nucleotide discovered"
  in
  String.to_seq s |> List.of_seq |> List.map f
;;

let hamming_distance (a : nucleotide list) (b : nucleotide list) : (int, string) Result.t =
  let rec accu (a : nucleotide list) (b : nucleotide list) (n : int) : int =
    match a, b with
    | ah :: at, bh :: bt -> if not (ah == bh) then accu at bt (n + 1) else accu at bt n
    | _, _ -> n
  in
  match List.is_empty a, List.is_empty b with
  | true, false -> Error "left strand must not be empty"
  | false, true -> Error "right strand must not be empty"
  | _, _ ->
    if List.length a <> List.length b
    then Error "left and right strands must be of equal length"
    else Ok (accu a b 0)
;;

let hamdist a b = hamming_distance (dna_of_string a) (dna_of_string b);;

let lines = In_channel.with_open_text "./data/rosalind_hamm.txt" In_channel.input_lines in
let l1 = List.nth lines 0 in
let l2 = List.nth lines 1 in
print_int (Result.get_ok (hamdist l1 l2));
print_endline
