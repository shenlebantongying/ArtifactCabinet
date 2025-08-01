(* ::Package:: *)

data=ToExpression/@StringSplit[Import["./data/rosalind_tree.txt"]];
total=First@data;
edges=Rest@data;


g=Graph[MapApply[UndirectedEdge[#1,#2]&,Partition[edges,2]]]


singleNodes=Complement[Range[1,total],edges]


Length@ConnectedComponents[g]+Length@singleNodes-1
