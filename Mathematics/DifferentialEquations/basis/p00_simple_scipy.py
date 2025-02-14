# (4+t^2) dy/dt + 2ty = 4 t

# dy/dt = (4 t - 2ty) / (4+t^2)

import scipy as sp
import numpy as np
import matplotlib.pyplot as plt


def f(t, y):
    return (4 * t - 2 * t * y) / (4 + t**2)


# solve_ivp -> initial value problem
res = sp.integrate.solve_ivp(
    f,
    t_span=(0, 10),
    y0=[
        1,
    ],
    t_eval=np.linspace(0, 10, 100),
)

plt.scatter(res.t, res.y[0])
plt.show()
