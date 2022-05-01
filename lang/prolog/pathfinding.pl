%% Prolog for Ai 4th, Bratko
link(a,b).
link(b,d).
link(c,f).
link(d,f).
link(a,c).
link(c,d).
link(d,e).
link(f,a).

%% path (startNote, endNote)

%% 1. When start and end node are the same
path(Node, Node, [Node]).

%% 2. Where is a link from startNode to next, and the next eventually reach the endNode.
path(StartNode, EndNode, [ StartNode | Rest ]) :-
    link(StartNode, NextNode),
    path(NextNode, EndNode, Rest).

%% aux, basicly cons

conc([],L,L).
conc([X|L1],L2,[X|L3]) :-
    conc(L1,L2,L3).
%% conc([1,2,3],[4,5,6],L).
%% L = [1, 2, 3, 4, 5, 6].

/*

path(a,END,PATH).

The conc is a trick : it will force the length of conc to be [],[_],[_,_].

TODO: prolog: does _ has special meaning?

?- conc(L,_,_).
L = [] ;
L = [_] ;
L = [_, _] ;
L = [_, _, _] ;
L = [_, _, _, _] ;

conc(Path,_,_),path(a,e,Path).

*/
