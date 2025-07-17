# Goal:
# Draw two series stacked vertically with legends
# Draw mean value

# %%

import altair as alt
import polars as pl
import numpy as np

# %%
x = np.linspace(0, 4 * np.pi, 300)
d = pl.DataFrame({"x": x, "y1": np.sin(x) + 1, "y2": np.cos(x) - 1})
d
# %%
# Convert to "Long form"

d_long = d.unpivot(index="x")
d_long

# %%

base = alt.Chart(d_long)

c_facet = (
    base.mark_line()
    .encode(
        alt.X("x", axis=alt.Axis(labels=True)), alt.Y("value:Q"), color="variable:N"
    )
    .properties(width=200, height=100)
)
c_facet
# %%

## Transform data into format that can be used in the color key
rule = (
    base.transform_aggregate(
        mean="mean(value)",
    )
    .transform_fold(["mean"])
    .mark_rule()
    .encode(y=alt.Y("value:Q"), color="key:N")
)
rule

# %%

(c_facet + rule).facet(row="variable")
# %%
