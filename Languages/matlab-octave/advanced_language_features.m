%% Anonymous Functions

mySqr = @(x) x.^2;
disp(mySqr([1,2,3]));
mySqr(1,2,3);

%% Pass function as arg

mySum=@(a,b) (a+b);
result=myApply(mySum, 1,2);
disp(result);

%%