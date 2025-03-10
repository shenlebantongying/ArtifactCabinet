# Objective:
# - Randomly generate 2 series of normal distributed data
# - Plot a XY heatmap

# %%

import altair as alt
import polars as pl
import scipy.stats as stats
import numpy as np

alt.data_transformers.disable_max_rows()

# %%
d_wide = pl.DataFrame(
    {
        "a1": stats.norm(0, 10).rvs(10000),
        "a2": stats.norm(0, 10).rvs(10000),
    }
)
d_wide


# %%
base_wide = alt.Chart(d_wide)
# %%
# Hist gram on one dimension
base_wide.mark_bar().encode(alt.X("a1:Q", bin=True), alt.Y("count()"))
# %%
base_wide.mark_rect().encode(
    alt.X("a1:Q").bin(maxbins=50).scale(domain=[-50, 50]),
    alt.Y(
        "a2:Q",
    )
    .bin(maxbins=50)
    .scale(domain=[-50, 50]),
    alt.Color("count():Q", title="Count"),
)
