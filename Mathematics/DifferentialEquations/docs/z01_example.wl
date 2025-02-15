(* ::Package:: *)

f=x^2+y[x]^2+c*y[x]==0
g=D[eq,x]
F=Eliminate[{f,g},c]
sol=Solve[F,{y'[x]}]


(* For plotting, let p[x,y] be the slop of F at [x,y] *)
p[x_,y_]:=y'[x]/.sol/.y[x]->y//First
p[x,y]
StreamPlot[{1,p[x,y]},{x,-10,10},{y,-10,10}]



(* Example: two of the curves of the function `f` *)
Show[{ContourPlot[x^2+y^2+4y==0,{x,-10,10},{y,-10,10}], 
	  ContourPlot[x^2+y^2+-4y==0,{x,-10,10},{y,-10,10}]}]
