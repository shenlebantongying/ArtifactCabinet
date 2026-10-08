# %%
# Goal:
# aggregate multiple points's z data

import polars as pl

# %%
d1 = pl.DataFrame(
    {"x": [1, 2, 3],
     "y": [1, 2, 3],
     "z": [4, 5, 6]}
)

d2 = pl.DataFrame(
    {"x": [1, 2, 3],
     "y": [1, 2, 3],
     "z": [7, 8, 9]}
)

# %% method 1

c1 = pl.concat([d1, d2])
c1.group_by([pl.col("x"), pl.col("y")]).agg(pl.col("z").sum())

# %% method 2

j1 = (d1.join(d2, on=["x", "y"], how="left")
.select(
    pl.col("x"),
    pl.col("y"),
    pl.concat_list(pl.exclude("x", "y"))))
