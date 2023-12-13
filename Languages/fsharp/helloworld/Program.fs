// This file is to have a feel about most of the features of F# language
// Fsharp 4.7.0
module helloFsharp
open System

(* Aux functions *)

let square x = x * x
let negate x = -x

// ======== note: code below need maybe "template" like in C++?
let rec printList ilist=
    match ilist with
    | [] -> ()
    | h::t -> printf "%d " h
              printList t 
    

(* Pattern matching *)

let rec sum list =
    match list with
    | [] -> 0
    | h::t -> h + sum t

(* Data Types, mainly Collections *)

// Lists -> immutable collection of elements of same type
let list1 = ["b"; "c"]
let list2 = "a"::list1  //Prepending
let list3 = list1 @ list2

// Arrays -> fixed-sized, zero-based, mutable collection of consecutive data elements.

let array1 = [|"a";"b"|]
let first = array1.[0]

// Convent higher-order functions

let range_list = [1..2..10] // python: range
let for_list   = [ for i in 1.0..4.0 -> i**2.] // Python: list comprehension 

let init_list = List.init 5 (fun i -> 2 * i + 1) 
    

// Pipeline
let piped_x = 10 |> square |> square |> negate

[<EntryPoint>]
let main argv =
    printList init_list
    0 // return an integer exit code
