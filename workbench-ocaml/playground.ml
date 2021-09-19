#require "base";;
open Base

type nucleotide = A | C | G | T

let dna_of_string s =
  let f = function
    | 'A' -> A
    | 'C' -> C
    | 'G' -> G
    | 'T' -> T
    | _   -> failwith "Big news! New nucleotide discovered" in
  String.to_list s |> List.map ~f;;

let hamming_distance (a : nucleotide list) (b : nucleotide list):(int,Base.string) Result.t =
  let rec accu (a : nucleotide list) (b : nucleotide list) (n:int):int=
    match a,b with
    | ah::at,bh::bt -> if (not (phys_equal ah bh))
                    then accu at bt (n+1)
                    else accu at bt n
    | _,_ -> n
  in
  match List.is_empty a,List.is_empty b with
  | true,false -> Error "left strand must not be empty"
  | false,true -> Error "right strand must not be empty"
  | _,_ ->
    if (List.length a) <> (List.length b)
    then Error "left and right strands must be of equal length"
    else Ok (accu a b 0);;

let hamdist a b = hamming_distance (dna_of_string a) (dna_of_string b);;

hamdist "GGACGGATTCTG" "AGGACGGATTCT";;
