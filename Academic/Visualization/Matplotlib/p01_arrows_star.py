# %%
import matplotlib.pyplot as plt
import numpy as np

# %%
# Goal:
# Centering around (0,0)
# Plot a circle of arrows

deg_interval = 45
n = np.int64(360 / deg_interval)

X = np.zeros(n)
Y = np.zeros(n)

degrees = np.arange(0, n) * deg_interval

U = np.cos(np.deg2rad(degrees))
V = np.sin(np.deg2rad(degrees))
# %%
fig, ax = plt.subplots()

ax.quiver(X, Y, U, V, scale=10)

fig.savefig("p01_arrows.pdf")
