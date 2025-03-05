# %%
# Goal:
# Given data from 3 groups
# Provides a frequency table

# %%
import polars as pl

# %%
ab_enum = pl.Enum(["a", "b"])
d = pl.DataFrame(
    {
        "d1": ["a", "b", "a", "a", "a"],
        "d2": ["a", "a", "b", "b", "b"],
        "d3": ["b", "b", "a", "b", "a"],
    },
    schema_overrides={
        "d1": ab_enum,
        "d2": ab_enum,
        "d3": ab_enum,
    },
)
d
# %%
dm = d.unpivot()
dm
# %%

cm = dm.group_by(pl.all()).len()
cm
# %%

nd = (
    cm.pivot(on="variable", index="value", values="len")
    .sort("value")
    .select(pl.col("value"), pl.col("d1"), pl.col("d2"), pl.col("d3"))
)

# %%
nd.to_numpy()

# %%
