(* ::Package:: *)

s=StringJoin@StringSplit@Import["./data/rosalind_cons.txt"];
ns={"A","C","G","T"};
d=StringSplit[s,">Rosalind_"~~DigitCharacter..]//Characters;


counts=Map[n |-> Map[Count[#,n]&, d//Transpose],ns];


consensus=StringJoin[First/@ PositionLargest/@Transpose@counts/.MapThread[#1->#2&,{Range[1,4],ns}]]


countsStr=Map[ToString,counts,{2}];
tbl=MapThread[Prepend[#1,#2~~":"]&, {countsStr,ns}];
Map[StringRiffle[#," "]&,tbl]//Column
