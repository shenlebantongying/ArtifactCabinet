(* ::Package:: *)

Thread[f[{a, b}, {r, s}, {u, v}, {x, y}], List,{2,3}]


eq1=1 + 2 x + 3 x^2 == a + b x + c x^2
Apply[List,eq1]//First
CoefficientList[#, x] & /@ eq1
Thread[Reverse@%]


Manipulate[ListPlot[Array[Sin[2#]-Cos[3#]&,200,{0,range}]],{range,1,10}]


(*Aggreate sublist that share same first element*)
Part[#,2]& /@ #1 &/@Gather[{{a,1},{a,2},{b,3},{b,4},{b,5}},First[#1]==First[#2]&]


(*Tree graph*)
NestGraph[{f[#], g[#]} &, x,3, VertexLabels -> All]
