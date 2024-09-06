link(a,b).
link(b,c).
link(b,d).

yipp(A):-
    (A
    -> format("~w -> ~w\n",[A,ok]);
     format("~w -> ~w\n",[A,noop])).

connected(A,B) :-
    link(A,C), link(C,B).

:- initialization(main, main).
main() :-
    yipp(connected(a,c)),
    yipp(connected(a,a)),
    forall(connected(a,J), writeln(J)),
    halt.
