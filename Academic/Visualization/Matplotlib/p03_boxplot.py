# %%
import matplotlib.pyplot as plt
import numpy as np

# %%

fig, axs = plt.subplots(nrows=3)

# 1d sequence
axs[1].boxplot([[1, 2, 3], [2, 3, 4]])

# 2d array
axs[0].boxplot(np.array([[1, 2, 3], [2, 3, 4]]), tick_labels=["a", "b", "c"])
axs[2].boxplot([np.array([1, 2, 3]), np.array([2, 3, 4])])

fig.show()
