from sympy import *
init_printing()
x = symbols('x')
a=Integral(cos(x)*exp(x),x)

print(latex(a.doit()))
print(a.doit())