% the factorial of 0 is 1
% (as termination condition)
factorial(0,1).

% The factorial of N is F
factorial(N, F) :-
    N > 0,            
    N1 is N - 1,      
    factorial(N1,F1), % Factorial of N1 is F1
    F is N * F1.      % and F is N * F1

/*
Usage:

?- factorial(4,X).
X = 24
*/

/*
Execution trace
[trace]  ?- factorial(3,X).
   Call: (8) factorial(3, _5404) ? creep
   Call: (9) 3>0 ? creep
   Exit: (9) 3>0 ? creep
   Call: (9) _5626 is 3+ -1 ? creep
   Exit: (9) 2 is 3+ -1 ? creep
   Call: (9) factorial(2, _5628) ? creep
   Call: (10) 2>0 ? creep
   Exit: (10) 2>0 ? creep
   Call: (10) _5632 is 2+ -1 ? creep
   Exit: (10) 1 is 2+ -1 ? creep
   Call: (10) factorial(1, _5634) ? creep
   Call: (11) 1>0 ? creep
   Exit: (11) 1>0 ? creep
   Call: (11) _5638 is 1+ -1 ? creep
   Exit: (11) 0 is 1+ -1 ? creep
   Call: (11) factorial(0, _5640) ? creep
   Exit: (11) factorial(0, 1) ? creep
   Call: (11) _5644 is 1*1 ? creep
   Exit: (11) 1 is 1*1 ? creep
   Exit: (10) factorial(1, 1) ? creep
   Call: (10) _5650 is 2*1 ? creep
   Exit: (10) 2 is 2*1 ? creep
   Exit: (9) factorial(2, 2) ? creep
   Call: (9) _5404 is 3*2 ? creep
   Exit: (9) 6 is 3*2 ? creep
   Exit: (8) factorial(3, 6) ? creep
X = 6 .
*/