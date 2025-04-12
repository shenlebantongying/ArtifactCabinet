import scipy.stats as stats
import numpy as np

# Core idea: pretty much the standard normal distribution.
# Then obtain the area beyond certain z or x value via the CDF function

print(",", ",".join(map("{:.2f}".format, np.arange(0, 0.1, 0.01))))
for i in np.arange(0, 3.1, 0.1):
    print(f"{i:.1f}", end=",")
    row_results = map(lambda x: 1 - stats.norm.cdf(x), i + np.arange(0, 0.1, 0.01))
    print(",".join(map("{:.4f}".format, row_results)))
