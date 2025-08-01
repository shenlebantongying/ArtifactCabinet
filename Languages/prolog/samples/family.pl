% a fact stating albert is a male
male(albert).
male(edward).
% a fact stating alice is a female
female(alice).
female(victoria).
% a fact: albert is parent of edward
parent(albert,edward).
parent(victoria,edward).
% a rule: X is father of Y if X if a male parent of Y
father(X,Y) :-
    parent(X,Y), male(X).
% a similar rule for X being mother of Y
mother(X,Y) :- parent(X,Y), female(X).

/*
Usage:

swipl

?- consult(family). % load family.pl

?- father(albert, edward).
true.

% Note this true is deduced rather than computed or specified.

?- halt.

% _Variables_ begin with an uppercase letter or
with an underscore ’ x’

?- parent(WHO, edward).
WHO = albert ;
WHO = victoria.

% type ; for continuos querying
*/
