# Goal:
# Answer if 2 series are correlated.

# %%
import scipy.stats as stats
import numpy as np
# %%

x = np.array([340, 230, 405, 325, 280, 195, 265, 300, 350, 310], dtype=np.double)
y = np.array([71, 65, 83, 74, 67, 56, 57, 78, 84, 65], dtype=np.double)
n = len(x)

# %% compute manually

np.sum((x - np.mean(x)) * (y - np.mean(y))) / (
    (n - 1) * np.std(x, ddof=1) * np.std(y, ddof=1)
)

# %%

stats.pearsonr(x, y)
# %%
