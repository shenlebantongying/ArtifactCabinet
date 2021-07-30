(* Handy functions *)

fun swap(pr: int*bool) =
    (#2 pr,#1 pr);
(* val swap = fn : int * bool -> bool * int; *)

(* lst is xs -> x is singular, xs is plural *)
fun sum (lst:int list) =
    if null lst
    then 0
    else hd(lst) + sum (tl lst);
sum([1,2,3,4]);

fun pair_sum(xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + pair_sum(tl xs);
pair_sum([(1,1),(2,2),(3,3)]);

fun firsts (xs:(int * int) list) =
    if null xs
    then []
    else (#1 (hd xs))::(firsts(tl xs));
firsts([(1,2),(3,4),(5,6)]);


(* let -> local binding *)

(* python like range *)
fun range(start:int,over:int) =
    let fun count (from:int) =
        if from=over
        then over::[]
        else from::count(from+1)
    in
        count start
    end;
(* both are valid *)
range(5,10); 
range((2,4));


(* good style max *)

(* optional type in sml *)

(* TODO: optimization chance -> reduce the use of SOME *)

fun g_max (xs:int list) =
    if null xs
    then NONE
    else
        let val tl_ans = g_max(tl xs)
        in  if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME(hd xs)
    end; 
(* fn: int list -> int option *)

g_max([3,4,1,2]);


(* with Nested Patterns *)

(* TODO: generic zip function? *)
(* TODO: 2d arrary print function *)

exception BadTriple;

fun zip3 lst_trpl =
    case lst_trpl of
          ([],[],[]) => []
        | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
        | _ => raise BadTriple;

zip3([1,2,3,4],[5,6,7,8],[9,10,11,12]);

fun unzip3 lst =
    case lst of
          [] => ([],[],[])
        | (a,b,c)::tl=> let val (l1,l2,l3) = unzip3 tl
                        in  
                            (a::l1,b::l2,c::l3)
                        end;

unzip3 [(1,5,9),(2,6,10),(3,7,11),(4,8,12)];

(* Combine with datatypes *)

datatype exp = Const of int
             | Neg of exp
             | Add of exp * exp
             | Mul of exp * exp;

(* This is a syntax sugar, and it can be done by case *)
fun eval (Const i)     = i
  | eval (Neg e1)      = ~ (eval e1)
  | eval (Add (e1,e2)) = (eval e1) + (eval e2)
  | eval (Mul (e1,e2)) = (eval e1) * (eval e2);

eval(Add((Mul((Const 3),(Const 4))),(Const 2)));


(* Taking function as arg *)

fun repeat (f,n,x) =
    if n=0
    then x
    else f (repeat (f,n-1,x));

fun triple x = 3*x;

repeat(triple,4,2);
(* 2*3*3*3*3=162 *)

(* Anonymous functions *)

repeat((fn x => x*x),3,2);
(* ((2*2)*(2*2))*((2*2)*(2*2)) => 256*)

(* Higher order functions *)

map (fn x => x+1) [1,2,3];

(* returning functions *)

fun double_or_triple f =
    if f 9
    then fn x => 2*x
    else fn x => 3*x;
(* fn : (int -> bool) -> int -> int *)
(* Take a function that map an int to bool, 
then it will return a function that double_or_triple a number *)

double_or_triple (fn x => if x>5 then true else false) 3;
(* => 6, as 9 < 5, it wil be doubled *

