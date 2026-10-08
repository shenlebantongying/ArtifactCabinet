# %%
# Goal:
# Given data from 3 groups
# Provides a frequency table

# %%
import polars as pl

# %%
d1 = pl.DataFrame({"t": range(5), "d": ["a", "b", "a", "a", "a"]})
d2 = pl.DataFrame({"t": range(5), "d": ["a", "a", "b", "b", "b"]})
d3 = pl.DataFrame({"t": range(5), "d": ["b", "b", "a", "b", "a"]})

f1 = d1["d"].value_counts(name="f1_count")
f2 = d2["d"].value_counts(name="f2_count")
f3 = d3["d"].value_counts(name="f3_count")

f_all = f1.join(f2, on="d").join(f3, on="d").sort("d")
f_all

# %%
print(f_all.drop("d").to_numpy())

# %%
