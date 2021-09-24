(* Come from https://cs.wellesley.edu/~cs251/s20/assignments/parcon/*)
(* Lookup function come from https://github.com/wellesleycs251/sml-f20/blob/master/utils/Env.sml*)

fun lookup name env =
  (case List.find (fn (n,v) => n = name) env of
       SOME (n,v) => SOME v
     | NONE => NONE)

datatype calc_expr = CalcInt of int
                   | CalcAdd of calc_expr * calc_expr
                   | CalcSub of calc_expr * calc_expr
                   | CalcMul of calc_expr * calc_expr
                   | CalcDiv of calc_expr * calc_expr
                   | CalcBind of string * calc_expr * calc_expr
                   | CalcVar of string;

fun eval e env =
    let
        (* Try to evaluate e1 and e2, apply the operation f to their
           results, and return SOME of this value. If either e1 or e2
           results in NONE, return NONE instead. *)
        fun try (f, e1, e2) =
            case (eval e1 env, eval e2 env) of
                (SOME x, SOME y) => SOME (f (x, y))
              | _ => NONE
    in
        (
          case e of
              CalcInt i => SOME i
            | CalcAdd (e1, e2) => try (fn (x,y) => x + y, e1, e2)
            | CalcSub (e1, e2) => try (fn (x,y) => x - y, e1, e2)
            | CalcMul (e1, e2) => try (fn (x,y) => x * y, e1, e2)
            | CalcDiv (e1, e2) => try (fn (x,y) => x div y, e1, e2)
            | CalcBind (x, e, body) => (case eval e env of
                                            SOME y => eval body ((x, y) :: env)
                                          | NONE => NONE)
            | CalcVar x => lookup x env
        ) handle _ => NONE
    end

val calc_prog = CalcBind ("x", CalcAdd (CalcInt 21, CalcInt 2),
                          CalcDiv (CalcAdd (CalcInt 4,
                                            CalcMul (CalcInt 5, CalcVar "x")),
                                   CalcSub (CalcMul (CalcInt 17, CalcInt 3),
                                            CalcDiv (CalcInt 34, CalcVar "x"))))

(* Should evaluate to (SOME 2) *)
val calc_result = eval calc_prog []

(* x=21+2 *)
(* (4+(5*x))/((17*3)-(34/x)) = 2 (using prgrammer's div rule) *)
