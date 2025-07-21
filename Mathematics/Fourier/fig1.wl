(* ::Package:: *)

(*
Plot the periodic function f(x).
When x is (-Pi, Pi), f(x) = -t/2.
Expand this defination to +-infinities
*)
ClearAll[f]
f[x_ /; 0 <= x <= Pi] := -x/2
f[x_ /; Pi < x <= 2Pi] := -x/2+Pi
f[x_] := f[Mod[x,2Pi]]
fig1t=Plot[f[x],{x,-5Pi,5Pi},Ticks -> {Range[-5Pi, 5Pi, Pi], Automatic},AspectRatio->1/2]
Export["fig1_fx_theory.pdf",fig1t]
fig1t


ClearAll[f1]
f1[t_] := Sum[((-1)^k)/k * Sin[k*t],{k,1,10}] (* k should be infinity, but this makes plot obvious*)
fig1f=Plot[f1[t],{t,-5Pi,5Pi},Ticks -> {Range[-5Pi, 5Pi, Pi], Automatic},AspectRatio->1/2]
Export["fig1_fx_fourier.pdf",fig1f]
fig1f
