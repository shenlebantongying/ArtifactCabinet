using Symbolics
using Latexify

@variables x y

z = x^2 + y

Dx = Differential(x)

expand_derivatives(Dx(z))
