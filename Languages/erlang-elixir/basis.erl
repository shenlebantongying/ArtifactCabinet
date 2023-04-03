-module(basis).
%-export([hello_world/0,distance/2]).
-compile(export_all). % test only

hello_world() ->
    io:fwrite("hello,world\n").

distance(A,B) -> abs(A-B).

% pattern
% last expression is return value.
fac(0) -> 1;
fac(N) when N>0, is_integer(N) -> N*fac(N-1).

% atoms -> lower case words wth _ or @
% TODO: is atom simply constant value?
% variables -> upper case

% basis:fall_velocity(mars,100).
fall_velocity(Planet, Distance) when Distance >= 0  ->
  case Planet of
    earth -> math:sqrt(2 * 9.8 * Distance);
%   ^ atom doesn't need any declaration
    moon ->  math:sqrt(2 * 1.6 * Distance);
    mars ->  math:sqrt(2 * 3.71 * Distance)  % no closing period!
  end.

% data types
% list
type_list()->
  %concating
  io:format("~p \n",[[1,2,3]++[a,b,c]]),
  %substration
  % for each element in the second argument, the first occurrence of this
  % element (if any) is removed from the first argument.
  io:format("~p \n",[[a,b,c,c,d,c]--[c,c,a]]),

  % list comprehension
  io:format("~p \n",[[X||X<-[1,2,3,4,5],X>3]]),
  io:format("~p \n",[[{A,B}|| A<-[1,2,3],B<-[2,3]]]). 
  % => [{1,2},{1,3},{2,2},{2,3},{3,2},{3,3}] 
