(* ::Package:: *)

(*
Solve Example 1.

Remark:
The analytical solution given by mathematica is very complex.
*)

sol=DSolve[y'[x]==(4*x-x^3)/(4+y[x]^3),y[x],x]


Plot[y[x]/.sol/.Table[{C[1]->v},{v,-5,5,2}],{x,-5,5}]
