import scipy as sp
import numpy as np

# Goal: perform a t student test on the following conditions

# H0 ->
mu = 20

n = 10
std_env = 3.4
mean = 21.2


# get 10 samples from
dist = sp.stats.norm(loc=21.2, scale=3.4)
samples = dist.rvs(size=10)

print("samples ->", samples)
print(np.mean(samples))
print(np.std(samples))

ret = sp.stats.ttest_1samp(samples, mu)

# pvalue

print(ret)

# Convert p-value -> statistic
print(sp.stats.t.ppf(1 - ret.pvalue / 2, ret.df))

# Convert statistic -> p-value
# cdf -> accumulation of probability at for the statistic value
print(2 * (1 - sp.stats.t.cdf(np.abs(ret.statistic), ret.df)))

# depends on the p-value
# for 95% confidence, reject is (1-pvalue) < 0.95
