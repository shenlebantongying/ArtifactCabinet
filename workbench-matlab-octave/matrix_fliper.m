%% rainbow grid

%  0     1     2     3
%  1     2     3     4
%  2     3     4     5
%  3     4     5     6
%  4     5     6     7

% TODO -> 
[X,Y] = meshgrid(0:3,0:4);
Z=X+Y;
disp(Z);

plus_minus(1,2);