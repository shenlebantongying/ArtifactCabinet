(* Implemention of a mini callback mechanism *)

(* TODO: add a "unregister" to deactive a event *)

(* usage:

use "closure_callback.sml";

!timepressed;
onEvent 11;
onEvent 23;

if registered, you can see a print;
*)

(* cbs -> CallBackS *)
val cbs : (int -> unit) list ref = ref [];

fun onKeyEvent f = cbs := f::(!cbs);
(*                        ^ list appendex? *)

fun onEvent i =
    let fun loop fs =
        case fs of
              [] => ()
            | f::fst => (f i; loop fst)
    in loop (!cbs) end;

val timePressed = ref 0;
val _ = onKeyEvent (fn _ => timePressed := (!timePressed)+1);

fun printIfPressed i =
    onKeyEvent (fn j => if i=j
                        then print ("The key pressed" ^ Int.toString i ^ "\n")
                        else ());


(* register the keys you want *)
val _ = printIfPressed 4;
val _ = printIfPressed 11;
val _ = printIfPressed 23;

(* cbs will become *)
(* each fn will be checked & called *)
(* val it = ref [fn,fn,fn,fn] : (int -> unit) list ref *)


(* https://courses.cs.washington.edu/courses/cse341/19sp/unit3notes.pdf *)
