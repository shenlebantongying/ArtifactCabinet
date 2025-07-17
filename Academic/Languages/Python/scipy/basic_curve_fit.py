# Objective:
# Approximate sin(x) with a polynomial function
# Note that this only works when x value is small (number of sin periods is low).

# %%

import numpy as np
import scipy.optimize as opt
import matplotlib.pyplot as plt

# %%


def fun_guess(x, a, b, c, d, e, f, g):
    return a + b * x**2 + c * x**3 + d * x**4 + e * x**5 + f * x**6 + g * x**7


# %%

x = np.linspace(0, 15, 2000)
y = np.sin(x)
plt.plot(x, y)

# %%
ret = opt.curve_fit(fun_guess, x, y)
ret


# %%
def fun_guess_fitted(x):
    return fun_guess(x, *ret[0])


# %%
y_f = fun_guess_fitted(x)
y_f
# %%
plt.plot(x, y_f)
plt.plot(x, y)
