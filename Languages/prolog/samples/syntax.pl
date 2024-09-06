%%% Terms

cat(alice). % Alice is a cat
taller_than(tom, jarry) % Tom is taller than jarry

%%% Facts and Rules

% Rules: encode ways of deriving of computing new facts

%% code to english

% X is an animal if it is a cat
animal(X) :- cat(X).

% X is father of Y, if X is a parent of Y and X is male.
father(X,Y) :-
    parent(X,Y),male(X).

% X is taller than Y,
% if H1 is the hight of X, H2 is the Height of Y,
% and H1 is biggher than H2 
taller_than(X,Y) :-
    Height(X, H1), 
    Height(Y, H2),
    H1 > H2.

