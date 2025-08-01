print_pair([]).

print_pair([A]):-
    write(" Tail:"),
    write(A).

print_pair([A|B]) :-
    write("Head:"),
    write(A),
    print_pair(B). % Note that B is a list

/*
?- print_pair([a,b]).
Head:a Tail:b
true .
*/

/*
	[a, b, c] = [Head | Tail].	 % a = Head, [b, c] = Tail.
	[a, b] = [A, B | T].	     % a = A, b = B, [] = Tail.
	[a, B | C] = [X | Y].	     % a = X, [B | C] = Y.
*/


% ML-like style rec
rec_print([]).
rec_print([H|T]) :-
    write(H),
    write("\n"),
    rec_print(T).

/*
rec_print([1,2,3,4,[5,6,7]]).
1
2
3
4
[5,6,7]
true.
*/

writeln(X) :-
    write(X),
    write("\n").

% A better approach
fn_print(X) :-
    maplist(writeln,X).

/*
?- fn_print([1,2,3]).
1
2
3
true.
*/
