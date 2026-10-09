# ref @lay_linear_2022 -> 7.4 example 2

# %%

import numpy as np
import scipy

# %%

d = np.array([[4, 11, 14], [8, 7, -2]])
sol = scipy.linalg.svd(d)

(U, S, Vh) = sol

# reconstruct original matrix

s_diag = scipy.linalg.diagsvd(S, U.shape[0], Vh.shape[0])
U @ s_diag @ Vh
