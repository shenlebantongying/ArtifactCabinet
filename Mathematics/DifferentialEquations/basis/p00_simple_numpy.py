# (4+t^2) dy/dt + 2ty = 4 t

import sympy as sp
import numpy as np
import matplotlib.pyplot as plt

y = sp.symbols("y", cls=sp.Function)
t = sp.symbols("t")

myEq = sp.Eq((4 + t**2) * y(t).diff(t) + 2 * t * y(t), 4 * t)

result = sp.dsolve(myEq, y(t), ics={y(0): 1})

print(result)

numericalResults = []

for i in np.linspace(0, 10, 100):
    numericalResults.append(sp.N(result.rhs.subs(t, i)))

fig, ax = plt.subplots()
ax.plot(numericalResults, marker="o")
fig.show()
input()
