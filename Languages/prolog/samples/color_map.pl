%TODO: write a equivalent code in other language.
% in other language you generate every possibility by hand.


/*
Assuming we are coloring a map that neighboring regions have different color

r,g,b color

The four regions:

   ┌───┬───────┐
   │   │    B  │
   │ A ├───────┤
   │   │  ┌────┤
   │   │C │    │
   │   │  │  D │
   └───┴──┴────┘
*/

% define borders
border(r,g).
border(r,b).

border(g,r).
border(g,b).

border(b,r).
border(b,g).

% cap letter -> variable rather than atoms
coloring(A,B,C,D) :-
    border(A,B),
    border(A,C),
    border(B,C),
    border(C,D).

/*

Querying:

?- coloring(A,B,C,D).
A = D, D = r,
B = g,
C = b ;

A = r,
B = D, D = g,
C = b ;

A = D, D = r,
B = b,
C = g ;

A = r,
B = D, D = b,
C = g ;

A = g,
B = D, D = r,
C = b ;

..........

*/
