%% for

radMatrix=randi([0, 9], [3,3]);

disp("column-wise");
disp(radMatrix);
for x=radMatrix % this loop is
   disp(x);
   disp("end of loop");
end

disp("row-wise");
for x=radMatrix'%
   disp(x);
   disp("end of loop");
end

disp("ele-wise");
for x=radMatrix(:)
   disp(x);
end

%% GRIDs

% different view of point

% meshgrid -> consider the matrix as Cartesian coordinates
% -> for plotting
% x->horizontal
% y->vertical

% ndgrid -> consider matrix's dimension order
% -> to acess grid with (row,col)
% x-> rows
% y-> columns

%%
% plot x*e^(-x^2+-y^2)
[X,Y]=meshgrid(-3:0.1:3);
F = X .* exp(- X .^ 2 - Y .^ 2);
%        ^dot product

surf(X,Y,F)

%%
clear
% construct a 3-row 2-col matrix
Z=zeros(3,2);
[X,Y]=ndgrid(1:3,4:5);
for a=1:3
    for b=1:2
       Z(a,b)=X(a,b)+Y(a,b);
    end
end
