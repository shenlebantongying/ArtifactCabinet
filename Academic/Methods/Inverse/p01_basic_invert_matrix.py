# page 3 of
# Hansen, P. C. (2010). Discrete inverse problems: Insight and algorithms. Society for Industrial and Applied Mathematics.

import numpy as np
import scipy.linalg

A = np.array([[0.16, 0.10], [0.17, 0.11], [2.02, 1.29]])
b = np.array([0.27, 0.25, 3.33])

(solution, _, _, _) = scipy.linalg.lstsq(A, b)

print(f"diff: {scipy.linalg.norm(A @ solution - b)}")
