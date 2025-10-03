syms x ;
f = x^3 - 5

%%

x_sol = newton_sym(f,x,1)

%%
X = linspace(-5,5,20);
Y = arrayfun(@(x_variable) subs(f,x,x_variable), X);
plot(X,Y,"-o")
xline(x_sol,'-',{'Solution of f(x)=0'})
grid on
