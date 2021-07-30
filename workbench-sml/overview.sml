val const=5;

fun fact n =
    if n = 0 then 1
    else n*fact(n-1);

fact const;

val inc = fn x => x+1;
inc 2 * const; (*=> 15, inc 2 will be first executed*)

(* compond types *)

(* tuples *)
val x=("head",const div 2,true);
(* div -> int, / for real *)

val first = #1(x);


(* records *)
val cat = {name="Tom",age=10};
val catname = #name cat;

(* patterns *)

fun iszero(0) = true
  | iszero(_) = false;
iszero 1;
iszero 0;

val lst=[1,2,3,4];
val hd::tl=lst;                 (* warning *)
val [a,b,c,d]=lst;

(* types *)

type point = real * real;       (* simple alias or Abbreviations of atomic types *)

datatype btree = LEAF
               | NODE of int * btree * btree;

fun depth LEAF = 0
  | depth (NODE(_,t1,t2)) =
    Int.max (depth t1,depth t2)+1;

val testbtree = NODE(1,
                     NODE(2,
                          NODE(3,LEAF,LEAF),
                          NODE(4,LEAF,LEAF)),
                     NODE(5,LEAF,LEAF));
depth testbtree;

(* Simple Modules/Interfaces -- Structure/Signature *)

signature SHAPE =
sig
    val name:string;
    val show: unit -> unit;
end


(* A structure is an encapsulated, named, collection of declarations *)
structure point:SHAPE =
struct
val name="nice";
val id=1;          (* struct of a sig can have more things than the defination *)
fun show ()= (print name;
              print "\n")     (* multiple statements *)
end

(* Module functions -> Functors *)

functor shpfunc(X:SHAPE) =
struct
fun echo () = (print X.name;print "\n");
end

structure swf = shpfunc(point);

swf.echo();
