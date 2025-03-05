# %%
# Goal:
# Given data from 3 groups
# Provides a frequency table

# %%
import pandas as pd
# %%


d1 = pd.DataFrame({"t": range(5), "d1_v": ["a", "b", "a", "a", "a"]}).set_index("t")
d2 = pd.DataFrame({"t": range(5), "d2_v": ["a", "a", "b", "b", "b"]}).set_index("t")
d3 = pd.DataFrame({"t": range(5), "d3_v": ["b", "b", "a", "b", "a"]}).set_index("t")

d_all = pd.concat([d1, d2, d3], axis=1)
d_all
# %%
d_melt = d_all.melt()
d_melt
# %%
ct = d_melt.groupby(by="variable").value_counts().sort_index()
ct
# %%
print("Wrong ->", ct.to_numpy())
# %%
type(ct)

# %%

nd = pd.DataFrame({"a": ct[:, "a"], "b": ct[:, "b"]})
nd
# %%
nd.to_numpy().transpose()

# %%
