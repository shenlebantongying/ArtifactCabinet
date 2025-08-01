using OrdinaryDiffEq, Plots
#=
From 11th Elementary Differential Equations and Boundary Value Problems
Chapter 2 -> Example 1

(4+t^2) dy/dt + 2ty = 4 t

with initial condition y(0) = 1.

=#

f(u, p, t) = (4 * t - 2 * t * u) / (4 + t^2)

# analytical solution
sf(t) = (2t^2 + 4) / (4 + t^2)

prob = ODEProblem(f, 1, (0.0, 10.0))
sol = solve(prob, Tsit5())

plot(sol, linewidth=2, label="Numerical Solution")
plot!(sf, linewidth=2, label="analytical Solution", ls=:dash)
