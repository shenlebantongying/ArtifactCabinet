%%
% modeling
% dy/dt = t
% y = (t^2)/2

F = ode;
F.ODEFcn = @(t,y) t;
F.InitialValue = 0;

sol = solve(F,0,10);

plot(sol.Time, sol.Solution, "-o");
xlabel("t")
ylabel("y")

%%
