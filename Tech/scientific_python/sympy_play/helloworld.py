import sympy as sym

sym.init_printing()
x = sym.symbols("x")
a = sym.Integral(sym.cos(x) * sym.exp(x), x)

print(sym.latex(a.doit()))
print(a.doit())
