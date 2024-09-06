% Note: First X -> the result

:- begin_tests(sth).

% Last element
last_e(X,[X]).
last_e(X,[_|L]) :- last_e(X,L).

test(last_e) :- last_e(X,[1,2,3,4]) ->  X==4.


% last but one
last_bo(X,[X,_]).
last_bo(X,[_|L]) :- last_bo(X,L).

test(last_bo) :- last_bo(X,[1,2,3,4]) ->  X==3.

% K'th element
element_at(X,[X|_],0).
element_at(X,[_|L],K):-
    NK is K-1,
    element_at(X,L,NK).

test(kth) :- element_at(X,[1,2,3,4,5],2) -> X==3.


% reverse
mrev(X,L) :- 
    mrev_helper(X,L,[]).

mrev_helper(X,[],X) :- !.
mrev_helper(X,[H|T],ACC) :-
    mrev_helper(X,T,[H|ACC]).

test(rev) :- mrev(X,[1,2,3,4]) -> X==[4,3,2,1].

:- end_tests(sth).

:- initialization(main, main).
main() :-
    run_tests,
    halt.

