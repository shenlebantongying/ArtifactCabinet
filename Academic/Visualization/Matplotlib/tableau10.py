# %%
import matplotlib.pyplot as plt
import numpy as np
import os

# %%
plt.style.use(f"{os.path.dirname(__file__)}/tableau10.mplstyle")
# %%
px = np.linspace(0, 1, 2)
plt.plot(px)
plt.plot(px + 1)
plt.plot(px + 2)
plt.plot(px + 3)
plt.plot(px + 4)
plt.plot(px + 5)
plt.plot(px + 6)
plt.plot(px + 7)

# %%
plt.style.available
# %%
os.getcwd()
# %%
