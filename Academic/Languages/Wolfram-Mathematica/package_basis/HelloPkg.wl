(* ::Package:: *)

BeginPackage["HelloPkg`"];


GetV::usage = "Get Some V"
MyPlus::usage = "New Kind of Plus"


Begin["`Private`"];
v = 10;
GetV[] := v
MyPlus[x_,y_] := x+y
End[];


EndPackage[];
