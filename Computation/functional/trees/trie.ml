(* aka prefix trees *)
(* aka reTRIEval *)

(* TODO: formalize this *)

(*
  representation of and & ant & anti trie

  bool means word stops.
  
  * empty -> <false, [a]>
  * |
  * a -> <false, [n]>
  * |
  * n -> <false, [d, t]>
  * |\
  * | d -> <true, []>
  * |
  * t -> <true, [i]>
  * |
  * i -> <true, []>

  q -> indicating this is a word stop
  ss -> ('a * 'a trie) list -> assoc list of subtrees
 *)

type 'a trie = N of bool * ('a * 'a trie) list;;

let empty = N(false,[]);;

let rec memq_trie (w:'a list) (t: 'a trie) : bool =
  match w, t with
    [], N(q, _) -> q
  | wh::wt, N(_,ss) ->
     let rec find (ss: ('a * 'a trie) list) =
       match ss with
         [] -> false
       | (j,cd)::cr ->
          if j = wh then (memq_trie wt cd)
          else find cr
     in find ss;;

let rec add_trie (w:'a list) (t: 'a trie): 'a trie =
  match w, t with
    [], N(_,ss) -> N(true, ss)
  | wh::wt, N(q,ss) ->
     let rec add_to (ss:('a * 'a trie) list) : ('a * 'a trie) list =
       match ss with
         [] -> [(wh, add_trie wt empty)]
       | (j,cd)::cr ->
          if j = wh then (wh,add_trie wt cd)::cr
          else (j,cd)::(add_to cr)
     in N(q,add_to ss)
;;


let t_and = ['a';'n';'d'];;
let t_ant = ['a';'n';'t'];;
let t_anti = ['a';'n';'t';'i'];;
let t_ana = ['a';'n';'a'];;

let sampleTrie = N(false,[('a',N(false,[('n',N(false, [('d',N(true, []))])) ]))]);;


memq_trie t_and sampleTrie;;
memq_trie t_ant sampleTrie;;

memq_trie t_and (add_trie t_and empty);;

add_trie t_ant empty;;

memq_trie t_ant (add_trie t_and (add_trie t_ant empty));;

add_trie ['a']  empty;;

(*
  2 stops in 1 branch
  
  stop 1
  v  
  a  n  t  i
  ^ stop 2
 *)

let coupledTrie =  add_trie t_ant (add_trie t_anti empty);;

memq_trie t_ant coupledTrie;;
memq_trie t_anti coupledTrie;;

(* and & ant & anti *)

let triTrie = add_trie t_anti (add_trie t_and (add_trie t_ant empty));;

memq_trie t_ant triTrie;;
memq_trie t_anti triTrie;;
memq_trie t_and triTrie;;
memq_trie t_ana triTrie;;
