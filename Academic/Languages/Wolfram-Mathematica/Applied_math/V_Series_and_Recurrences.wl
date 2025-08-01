(* ::Package:: *)

(*Fibonacci recurrence*)


(* Recrusion: 0 means the function itself *)
fib1=(If[#1==1||#1==2,1,#0[#1-1]+#0[#1- 2]])&
fib1[10]


(*Linear Algebra hacking*)
fib2[n_]:=Union @@ NestList[{{0,1},{1,1}}.# &, {1, 1}, n-2]
fib3[n_]:=MatrixPower[{{1,1},{1,0}},n-1]
fib2[10]
fib3[10]


(*LinearRecurrence*)
(* y[n+2]=6y[n+1]-y[n], y[0]=1,y,[1]=3*)
LinearRecurrence[{6,-1},{1,3},10]
