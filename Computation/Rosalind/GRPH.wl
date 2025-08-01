(* ::Package:: *)

labled=Reverse/@Import["./data/rosalind_grph.txt",{"FASTA","LabeledData"}];
seqs=Map[First,labled/. Rule->List]


endings=StringTake[#,-3]& /@ seqs


edges=
Flatten[#,1]& @
  MapThread[
    {seq,end} |-> Map[{seq,#}&,Select[seqs,s |-> StringMatchQ[s,StartOfString~~end~~__]]],
    {seqs,endings}]


MapApply[
  StringJoin[#1," ",#2]&,
  ReplaceAll[edges,labled]]//Column
