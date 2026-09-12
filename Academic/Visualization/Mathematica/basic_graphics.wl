(* ::Package:: *)

(* TODO: uses graphics transformation instead *)


Graphics[
	Join[
		Table[{RGBColor[EvenQ[x]/.{True->1, False->0},0,0],Circle[{0,0},x]},{x,0,10}],
		{Blue},
		Table[Line@{{0,0},AngleVector[{deg/360*10*1.5,deg Degree}]},{deg,0,360,10}]],
	GridLines->Automatic,Axes->True]
