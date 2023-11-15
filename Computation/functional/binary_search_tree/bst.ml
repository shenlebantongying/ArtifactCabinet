(*
  
  > A ::= *         Leaf
  >     | <x,A1,A2> Node

  
  Note that there is alternative representation
  
  > A ::= [x]       Leaf
  >     | <A1,A2> Node
  
 *)

exception NOPE;;

(* B -> Branch, L -> Leaf *)
type 'a tree =
  B of 'a * 'a tree * 'a tree
| L;;

let rec size = function
    B (_, l, r) -> 1 + size l + size r
  | L -> 0;;

let rec maxDepth = function
    B (_, l, r) -> 1 + max (maxDepth l) (maxDepth r)
  | L -> 0;;

let rec flatten = function
    B (a,l,r) -> flatten l @ [a] @ flatten r
  | L -> [];;

let rec lookup t k =
  match t with
    L -> None
  | B ((k', v), l, r) ->
     if k = k' then Some v
     else if k < k' then lookup l k
     else lookup r k;;

let rec insert k t = 
  match t with
    L -> B (k, L, L)
  | B(k', l, r) -> 
     if k < k' then B (k', insert k l, r)
     else if k > k' then B (k', l, insert k r)
     else B(k', l, r);;

let rec insertKV k v t =
  match t with
    L -> B ((k,v), L, L)
  | B ((k',v'), l, r) ->
     if k = k' then B ((k,v),l,r)
     else if k < k' then B ((k',v'), insertKV k v l, r)
     else B ((k',v'), l, insertKV k v r);;

(*
  impl del y

  -> find the tree that contains y -> <y, A, C>
  -> del y
  -> (join) uses the smallest of C as new root then attach back
 *)

let rec min_tree t = 
  match t with
    L -> raise NOPE
  | B (x,L,_) ->  x
  | B (_,l,_) -> min_tree l;;

let rec delMin t =
  match t with
    L -> raise NOPE
  | B(_,L,r) -> r;
  | B(x,l,r) -> B(x,(delMin l),r);;

let rec join t1 t2 =
  match t1,t2 with
    t1,L-> t1
  | L,t2-> t2
  |t1,t2 -> B(min_tree t2,t1,delMin t2);;

let rec del k t =
  match t with
    L -> L
  | B(k',l,r)->
     if k = k' then join l r
     else if k < k' then B (k',del k l,r)
     else B(k',l,del k r);;

(* Tests *)

let sampleTree = insert 1 ( insert 2 (insert 3 (insert 4 L)));;

let kvTree = insertKV 1 "one" (insertKV 2 "two" (insertKV 3 "three" ( insertKV 4 "four" L)));;

size sampleTree;;

maxDepth sampleTree;;

flatten sampleTree;;

flatten kvTree;;                (* still works *)
lookup kvTree 3;;

min_tree sampleTree;;

delMin sampleTree;;

del 2 sampleTree;;

