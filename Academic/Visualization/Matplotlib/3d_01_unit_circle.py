# %%
import matplotlib.pyplot as plt
import numpy as np
# %%
# Goal: plot a half sphere.
#
# z = sqrt(r^2 - x^2 - y^2)

r = 10.0
x = np.linspace(-r, r, 1000)
y = np.linspace(-r, r, 1000)
X, Y = np.meshgrid(x, y)
Z = np.sqrt(np.maximum(0, np.power(r, 2) - np.power(X, 2) - np.power(Y, 2)))

# %%
fig, ax = plt.subplots(subplot_kw={"projection": "3d"})

ax.contour(X, Y, Z)

plt.show()
