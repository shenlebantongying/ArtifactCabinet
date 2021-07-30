%% Simple Second Order ODE

% y''=9x

syms y(x)
mySimpleODE = diff(y,x) == 9*x;
ySol(x)=dsolve(mySimpleODE);

disp(ySol);    % => (9*x^2)/2+C1


C1=3;
disp(eval(ySol(2)));% => C1+18
                    % This is a hidden assignment :(
clear C1;
%%