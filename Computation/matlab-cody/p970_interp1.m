% https://www.mathworks.com/matlabcentral/cody/groups/12/problems/970

% idea: specify xq points as required

x = [1 1 ; 3 0 ; 1 1];
y = [5 3];

y=y-1; % remove duplicates

xx = [1 cumsum(y)]
xq = 1:sum(y)

interp1(xx,x,1:sum(y))
