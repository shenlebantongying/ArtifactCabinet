(* ::Package:: *)

n=ToExpression@Import["./data/rosalind_perm.txt"]


p=Permutations[Range[1,n]]


Length[p]
StringRiffle[Map[StringRiffle[#," "]&,Map[ToString,p,{2}]],"\n"]
CopyToClipboard[%]
